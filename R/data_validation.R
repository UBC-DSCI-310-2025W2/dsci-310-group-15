

#' Read and validate .RDS dataframe
#'
#' @param path Path to an RDS file.
#' @param data_name Name used in report details.
#'
#' @return A dataframe
load_validated_rds_data_frame <- function(path, data_name = "data") {
  processandplot::assert_single_string(path, "path")
  processandplot::assert_single_string(data_name, "data_name")

  has_rds_extension <- grepl("\\.RDS$", path, ignore.case = TRUE)
  path_exists <- file.exists(path)
  loaded_data <- NULL
  load_error <- NULL

  if (path_exists) {
    loaded_data <- tryCatch(
      readRDS(path),
      error = function(error) {
        load_error <<- conditionMessage(error)
        NULL
      }
    )
  }

  is_valid_data_frame <- is.data.frame(loaded_data)

  if (!has_rds_extension) {
    stop(sprintf("`%s` must be supplied as an .RDS file.", data_name), call. = FALSE)
  }

  if (!path_exists) {
    stop(sprintf("Input file not found: %s", path), call. = FALSE)
  }

  if (!is.null(load_error)) {
    stop(
      sprintf("Failed to read `%s` from %s: %s", data_name, path, load_error),
      call. = FALSE
    )
  }

  if (!is_valid_data_frame) {
    stop(
      sprintf("`%s` must be a data frame stored in an .RDS file.", data_name),
      call. = FALSE
    )
  }

  loaded_data

}

#' Expected raw Steam columns used for modeling
#'
#' @return Character vector of raw required columns.
raw_steam_required_columns <- function() {
  c(
    "appid",
    "name_from_applist",
    "app_details.data.required_age",
    "app_details.data.is_free",
    "app_details.data.type",
    "app_details.data.release_date.date",
    "app_details.data.platforms.windows",
    "app_details.data.platforms.mac",
    "app_details.data.platforms.linux",
    "app_details.data.categories",
    "app_details.data.dlc",
    "app_details.data.demos",
    "app_details.data.developers",
    "app_details.data.publishers"
  )
}

#' Expected modeled Steam columns before one-hot category indicators
#'
#' @return Character vector of required modeling columns.
modeling_required_columns <- function() {
  c(
    "game_id",
    "game_name",
    "is_free",
    "required_age",
    "release_year",
    "game_type",
    "windows_support",
    "mac_support",
    "linux_support",
    "platform_count",
    "has_dlc",
    "has_demo",
    "n_categories",
    "developer_name",
    "publisher_name"
  )
}

#' Known Steam `type` values for this analysis
#'
#' @return Character vector of allowed type values.
known_steam_game_types <- function() {
  c(
    "advertising",
    "demo",
    "dlc",
    "episode",
    "game",
    "mod",
    "music",
    "series",
    "video",
    "unknown"
  )
}

#' Validate raw Steam source data
#'
#' @param games_data Raw Steam data frame.
#' @param stage Pipeline stage label.
#' @param missing_threshold Maximum allowed missingness for required app-detail fields.
#' @param min_class_prop Minimum allowed target class proportion.
#'
#' @return Validation report data frame.
validate_raw_games_data <- function(
  games_data,
  stage = "raw_source",
  missing_threshold = 0.15,
  min_class_prop = 0.05) {
    if (!is.data.frame(games_data)) {
      stop("`games_data` must be a data frame.", call. = FALSE)
    }

    required_columns <- raw_steam_required_columns() 

    atomic_required_cols <- c(
    "app_details.data.required_age",
    "app_details.data.is_free",
    "app_details.data.type",
    "app_details.data.release_date.date",
    "app_details.data.platforms.windows",
    "app_details.data.platforms.mac",
    "app_details.data.platforms.linux"
  )

  list_cols <- c(
    "app_details.data.categories",
    "app_details.data.dlc",
    "app_details.data.demos",
    "app_details.data.developers",
    "app_details.data.publishers"
  )

  character_cols <- c(
    "name_from_applist",
    "app_details.data.required_age",
    "app_details.data.type",
    "app_details.data.release_date.date"
  )

  logical_cols <- c(
    "app_details.data.is_free",
    "app_details.data.platforms.windows",
    "app_details.data.platforms.mac",
    "app_details.data.platforms.linux"
  )

  games_data %>%
    pointblank::col_exists(columns = dplyr::all_of(required_columns)) %>%
    pointblank::col_is_integer(columns = "appid") %>%
    pointblank::col_is_character(columns = dplyr::all_of(character_cols)) %>%
    pointblank::col_is_logical(columns = dplyr::all_of(logical_cols)) %>%
    pointblank::specially(
      fn = function(x) {
        all(vapply(
          list_cols, 
          function(nm) is.list (x[[nm]]) && !is.data.frame(x[[nm]]),
          logical(1)
        ))
      },
      label = "raw list columns are list columns "
    ) %>%
    pointblank::rows_complete(columns = c("appid", "name_from_applist")) %>%
    pointblank::col_vals_expr(
      expr = ~ !is.na(name_from_applist) & nzchar(trimws(name_from_applist))
    ) %>%
    pointblank::col_vals_not_null(
      columns = dplyr::all_of(atomic_required_cols),
      actions = pointblank::stop_on_fail(stop_at = missing_threshold)
    ) %>%
    pointblank::rows_distinct(columns = "appid") %>%
    pointblank::specially(
      fn = function(x) {
        all(
          is.na(x$app_details.data.is_free) |
          x$app_details.data.is_free %in% c(TRUE, FALSE)
        )
      },

      label = "non-missing raw target values are TRUE or FALSE"
    ) %>%
    pointblank::specially(
      fn = function(x) {
        all(
          is.na(x$app_details.data.type) |
            x$app_details.data.type %in% known_steam_game_types()
        )
      },
      label = "non-missing raw type values match known Steam types"
    ) %>%
    pointblank::specially(
      fn = function(x) {
        props <- prop.table(table(x$app_details.data.is_free, useNA = "no"))
        length(props) == 2L && min(props) >= min_class_prop
      },
      label = "raw target distribution is acceptable"
    )
}

#' Validate a wrangled modeling table
#'
#' @param modeling_data Modeling data frame.
#' @param stage Pipeline stage label.
#' @param expected_category_count Optional expected number of `cat_` columns.
#' @param optional_metadata_fail_threshold Missingness threshold for optional metadata.
#' @param min_class_prop Minimum target class proportion.
#' @param current_year Current year for release-year range checks.
#'
#' @return Validation report data frame.
validate_modeling_table <- function(
    modeling_data,
    stage = "modeling_table",
    expected_category_count = NULL,
    optional_metadata_fail_threshold = 0.20,
    min_class_prop = 0.05,
    current_year = as.integer(format(Sys.Date(), "%Y"))) {
      if (!is.data.frame(modeling_data)) {
        stop("`modeling_data` must be a data frame.", call. = FALSE)
      }

  required_columns <- modeling_required_columns()
  category_columns <- grep("^cat_", names(modeling_data), value = TRUE)

  predictor_columns <- c(
    "is_free",
    "required_age",
    "release_year",
    "game_type",
    "windows_support",
    "mac_support",
    "linux_support",
    "platform_count",
    "has_dlc",
    "has_demo",
    "n_categories",
    category_columns
  )

  warn_at_one_missing <- if (nrow(modeling_data) > 0L) 1 / nrow(modeling_data) else 1

  modeling_data %>%
    pointblank::col_exists(columns = dplyr::all_of(required_columns)) %>%
    pointblank::specially(
      fn = function(x) {
        n_cat <- sum(grepl("^cat_", names(x)))
        if (is.null(expected_category_count)) {
          n_cat > 0L
        } else {
          n_cat == as.integer(expected_category_count)
        }
      },
      label = "category indicator columns present"
    ) %>%
    pointblank::col_is_integer(
      columns = c("game_id", "required_age", "platform_count", "n_categories")
    ) %>%
    pointblank::col_is_numeric(columns = "release_year") %>%
    pointblank::col_is_character(columns = c("game_name", "developer_name", "publisher_name")) %>%
    pointblank::col_is_factor(columns = c("is_free", "game_type")) %>%
    pointblank::col_is_logical(
      columns = dplyr::all_of(c(
        "windows_support",
        "mac_support",
        "linux_support",
        "has_dlc",
        "has_demo",
        category_columns
      ))
    ) %>%
    pointblank::rows_complete(columns = c("game_id", "game_name")) %>%
    pointblank::col_vals_expr(
      expr = ~ !is.na(game_name) & nzchar(trimws(game_name))
    ) %>%
    pointblank::rows_complete(columns = dplyr::all_of(predictor_columns)) %>%
    pointblank::col_vals_not_null(
      columns = c("developer_name", "publisher_name"),
      actions = pointblank::action_levels(
        warn_at = warn_at_one_missing,
        stop_at = optional_metadata_fail_threshold
      )
    ) %>%
    pointblank::rows_distinct(columns = "game_id") %>%
    pointblank::col_vals_between(columns = "required_age", left = 0, right = 18) %>%
    pointblank::specially(
      fn = function(x) {
        all(x$release_year == -1 | (x$release_year >= 2000 & x$release_year <= current_year + 10))
      },
      label = "release_year values are -1 or within the expected analysis range"
    ) %>%
    pointblank::col_vals_between(columns = "platform_count", left = 1, right = 3) %>%
    pointblank::col_vals_between(columns = "n_categories", left = 0, right = 30) %>%
    pointblank::col_vals_in_set(columns = "is_free", set = c("Free", "Paid")) %>%
    pointblank::col_vals_in_set(columns = "game_type", set = known_steam_game_types()) %>%
    pointblank::specially(
      fn = function(x) {
        cat_cols <- grep("^cat_", names(x), value = TRUE)
        length(cat_cols) > 0L &&
          all(vapply(
            cat_cols,
            function(nm) length(unique(stats::na.omit(x[[nm]]))) > 1L,
            logical(1)
          ))
      },
      label = "category indicators are not constant"
    ) %>%
    pointblank::specially(
      fn = function(x) {
        props <- prop.table(table(x$is_free, useNA = "no"))
        length(props) == 2L && min(props) >= min_class_prop
      },
      label = "model target distribution is acceptable"
    )
}


#' Compute a safe absolute correlation
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#'
#' @return Absolute correlation or `NA_real_` when not computable.
safe_abs_cor <- function(x, y) {
  complete <- stats::complete.cases(x, y)
  if (sum(complete) < 3L) {
    return(NA_real_)
  }

  x <- x[complete]
  y <- y[complete]
  if (length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(NA_real_)
  }

  abs(stats::cor(x, y))
}

#' build a feature matrix for correlation checks
#' 
#' @param train_data training datafram
#' @param predictors vector of predictor column names
#' 
#' @return a dataframe
training_feature_matrix <- function(train_data, predictors) {
  processandplot::validate_required_columns(train_data, predictors, "train_data")

  feature_df <- train_data[, predictors, drop = FALSE]
  mm <- stats::model.matrix(~ . - 1, data = feature_df)

  as.data.frame(mm, stringsAsFactors = FALSE)
}

#' find maximum absolute correlation between target & features
#' 
#' @param train_data training dataframe
#' @param predictors vector of predictor columns
#' @param target_col targt column name
#' @param positive_class the positive class for correlation checks
#' 
#' @return a number 
max_abs_target_correlation <- function(
    train_data,
    predictors,
    target_col = "is_free",
    positive_class = "Free") {
  processandplot::validate_required_columns(train_data, c(target_col, predictors), "train_data")

  x_df <- training_feature_matrix(train_data, predictors)
  y <- as.numeric(as.character(train_data[[target_col]]) == positive_class)

  cors <- vapply(
    x_df,
    function(col) safe_abs_cor(col, y),
    numeric(1)
  )

  if (length(cors) == 0L || all(is.na(cors))) {
    return(0)
  }

  max(cors, na.rm = TRUE)
}

#' get the max absolute correlation between features
#' 
#' @param train_data training dataframe
#' @param predictors vector of predictor column names
#' 
#' @return a number 
max_abs_feature_correlation <- function(train_data, predictors) {
  processandplot::validate_required_columns(train_data, predictors, "train_data")

  x_df <- training_feature_matrix(train_data, predictors)

  if (ncol(x_df) < 2L) {
    return(0)
  }

  cor_mat <- suppressWarnings(abs(stats::cor(x_df, use = "pairwise.complete.obs")))
  cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA_real_

  vals <- as.numeric(cor_mat)
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0L) {
    return(0)
  }

  max(vals)
}


#' Validate train-only correlation checks
#'
#' @param train_data Training data frame only.
#' @param predictors Predictor columns to check.
#' @param target_col Target column.
#' @param positive_class Target class encoded as 1 for correlation checks.
#' @param target_warn_threshold Warning threshold for target-feature correlations.
#' @param target_fail_threshold Failure threshold for target-feature correlations.
#' @param feature_warn_threshold Warning threshold for feature-feature correlations.
#' @param feature_fail_threshold Failure threshold for feature-feature correlations.
#' @param stage Pipeline stage label.
#'
#' @return Validation report data frame.
validate_training_correlations <- function(
    train_data,
    predictors,
    target_col = "is_free",
    positive_class = "Free",
    target_warn_threshold = 0.80,
    target_fail_threshold = 0.95,
    feature_warn_threshold = 0.90,
    feature_fail_threshold = 0.98,
    stage = "training_split") {
      if (!is.data.frame(train_data)) {
        stop("`train_data` must be a data frame.", call. = FALSE)
      }
        
      if (!is.character(predictors) || length(predictors) == 0L) {
        stop("`predictors` must be a non-empty character vector.", call. = FALSE)
      }
      
      processandplot::validate_required_columns(train_data, c(target_col, predictors), "train_data")

  train_data %>%
    pointblank::col_exists(columns = dplyr::all_of(c(target_col, predictors))) %>%
    pointblank::specially(
      fn = function(x) {
        max_abs_target_correlation(
          x,
          predictors = predictors,
          target_col = target_col,
          positive_class = positive_class
        ) < target_warn_threshold
      },
      actions = pointblank::warn_on_fail(),
      label = "target-feature correlations below warning threshold"
    ) %>%
    pointblank::specially(
      fn = function(x) {
        max_abs_target_correlation(
          x,
          predictors = predictors,
          target_col = target_col,
          positive_class = positive_class
        ) < target_fail_threshold
      },
      label = "target-feature correlations below failure threshold"
    ) %>%
    pointblank::specially(
      fn = function(x) {
        max_abs_feature_correlation(x, predictors = predictors) < feature_warn_threshold
      },
      actions = pointblank::warn_on_fail(),
      label = "feature-feature correlations below warning threshold"
    ) %>%
    pointblank::specially(
      fn = function(x) {
        max_abs_feature_correlation(x, predictors = predictors) < feature_fail_threshold
      },
      label = "feature-feature correlations below failure threshold"
    )
}

validation_report_row <- function(stage, check, status, observed_value, threshold, details) {
  data.frame(
    stage = stage,
    check = check,
    status = status,
    observed_value = observed_value,
    threshold = threshold,
    details = details,
    stringsAsFactors = FALSE
  )
}

format_validation_number <- function(x) {
  sprintf("%.3f", x)
}

format_validation_rates <- function(x) {
  if (length(x) == 0L) {
    return("none")
  }

  paste0(names(x), "=", format_validation_number(as.numeric(x)), collapse = ", ")
}

clean_model_matrix_term <- function(term) {
  sub("TRUE$", "", term)
}

format_top_named_correlations <- function(x, n = 5L) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return("none")
  }

  x <- sort(x, decreasing = TRUE)
  x <- head(x, n)
  names(x) <- clean_model_matrix_term(names(x))
  format_validation_rates(x)
}

top_feature_correlation_pairs <- function(train_data, predictors, n = 5L) {
  x_df <- training_feature_matrix(train_data, predictors)

  if (ncol(x_df) < 2L) {
    return(data.frame(feature_1 = character(), feature_2 = character(), value = numeric()))
  }

  cor_mat <- suppressWarnings(abs(stats::cor(x_df, use = "pairwise.complete.obs")))
  cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA_real_
  pairs <- which(!is.na(cor_mat), arr.ind = TRUE)

  if (nrow(pairs) == 0L) {
    return(data.frame(feature_1 = character(), feature_2 = character(), value = numeric()))
  }

  result <- data.frame(
    feature_1 = clean_model_matrix_term(rownames(cor_mat)[pairs[, 1]]),
    feature_2 = clean_model_matrix_term(colnames(cor_mat)[pairs[, 2]]),
    value = cor_mat[pairs],
    stringsAsFactors = FALSE
  )
  result <- result[order(result$value, decreasing = TRUE), , drop = FALSE]
  utils::head(result, n)
}

format_top_feature_correlation_pairs <- function(pairs) {
  if (nrow(pairs) == 0L) {
    return("none")
  }

  paste0(
    pairs$feature_1,
    " ~ ",
    pairs$feature_2,
    "=",
    format_validation_number(pairs$value),
    collapse = ", "
  )
}

build_modeling_validation_report <- function(
    modeling_data,
    stage = "model_input",
    expected_category_count = NULL,
    optional_metadata_fail_threshold = 0.20,
    min_class_prop = 0.05,
    current_year = as.integer(format(Sys.Date(), "%Y"))) {
  required_columns <- modeling_required_columns()
  category_columns <- grep("^cat_", names(modeling_data), value = TRUE)
  predictor_columns <- c(
    "is_free",
    "required_age",
    "release_year",
    "game_type",
    "windows_support",
    "mac_support",
    "linux_support",
    "platform_count",
    "has_dlc",
    "has_demo",
    "n_categories",
    category_columns
  )

  missing_required <- setdiff(required_columns, names(modeling_data))
  expected_category_label <- if (is.null(expected_category_count)) {
    "at least one cat_ column"
  } else {
    paste0(as.integer(expected_category_count), " cat_ columns")
  }
  has_expected_categories <- if (is.null(expected_category_count)) {
    length(category_columns) > 0L
  } else {
    length(category_columns) == as.integer(expected_category_count)
  }

  required_present <- intersect(required_columns, names(modeling_data))
  all_missing_required_rows <- if (length(required_present) == 0L || nrow(modeling_data) == 0L) {
    0L
  } else {
    sum(rowSums(is.na(modeling_data[required_present])) == length(required_present))
  }
  blank_game_ids <- if ("game_id" %in% names(modeling_data)) sum(is.na(modeling_data$game_id)) else NA_integer_
  blank_game_names <- if ("game_name" %in% names(modeling_data)) {
    sum(is.na(modeling_data$game_name) | !nzchar(trimws(modeling_data$game_name)))
  } else {
    NA_integer_
  }

  predictor_present <- intersect(predictor_columns, names(modeling_data))
  predictor_missingness <- if (length(predictor_present) > 0L) {
    vapply(modeling_data[predictor_present], function(col) mean(is.na(col)), numeric(1))
  } else {
    numeric()
  }
  max_predictor_missingness <- if (length(predictor_missingness) > 0L) {
    max(predictor_missingness)
  } else {
    NA_real_
  }

  optional_metadata <- intersect(c("publisher_name", "developer_name"), names(modeling_data))
  optional_missingness <- vapply(
    modeling_data[optional_metadata],
    function(col) mean(is.na(col) | !nzchar(trimws(col))),
    numeric(1)
  )
  optional_missingness <- sort(optional_missingness, decreasing = TRUE)
  optional_status <- if (length(optional_missingness) == 0L || max(optional_missingness) == 0) {
    "pass"
  } else if (max(optional_missingness) >= optional_metadata_fail_threshold) {
    "fail"
  } else {
    "warn"
  }

  type_mismatches <- character()
  if ("game_id" %in% names(modeling_data) && !is.integer(modeling_data$game_id)) type_mismatches <- c(type_mismatches, "game_id")
  if ("required_age" %in% names(modeling_data) && !is.integer(modeling_data$required_age)) type_mismatches <- c(type_mismatches, "required_age")
  if ("platform_count" %in% names(modeling_data) && !is.integer(modeling_data$platform_count)) type_mismatches <- c(type_mismatches, "platform_count")
  if ("n_categories" %in% names(modeling_data) && !is.integer(modeling_data$n_categories)) type_mismatches <- c(type_mismatches, "n_categories")
  if ("release_year" %in% names(modeling_data) && !is.numeric(modeling_data$release_year)) type_mismatches <- c(type_mismatches, "release_year")
  for (nm in intersect(c("game_name", "developer_name", "publisher_name"), names(modeling_data))) {
    if (!is.character(modeling_data[[nm]])) type_mismatches <- c(type_mismatches, nm)
  }
  for (nm in intersect(c("is_free", "game_type"), names(modeling_data))) {
    if (!is.factor(modeling_data[[nm]])) type_mismatches <- c(type_mismatches, nm)
  }
  for (nm in intersect(c("windows_support", "mac_support", "linux_support", "has_dlc", "has_demo", category_columns), names(modeling_data))) {
    if (!is.logical(modeling_data[[nm]])) type_mismatches <- c(type_mismatches, nm)
  }

  full_row_duplicates <- sum(duplicated(modeling_data))
  duplicate_game_ids <- if ("game_id" %in% names(modeling_data)) sum(duplicated(modeling_data$game_id)) else NA_integer_

  required_age_bad <- if ("required_age" %in% names(modeling_data)) sum(is.na(modeling_data$required_age) | modeling_data$required_age < 0 | modeling_data$required_age > 18) else NA_integer_
  platform_count_bad <- if ("platform_count" %in% names(modeling_data)) sum(is.na(modeling_data$platform_count) | modeling_data$platform_count < 1 | modeling_data$platform_count > 3) else NA_integer_
  n_categories_bad <- if ("n_categories" %in% names(modeling_data)) sum(is.na(modeling_data$n_categories) | modeling_data$n_categories < 0 | modeling_data$n_categories > 30) else NA_integer_
  release_year_bad <- if ("release_year" %in% names(modeling_data)) {
    sum(is.na(modeling_data$release_year) | !(modeling_data$release_year == -1 | (modeling_data$release_year >= 2000 & modeling_data$release_year <= current_year + 10)))
  } else {
    NA_integer_
  }

  target_values <- if ("is_free" %in% names(modeling_data)) unique(as.character(stats::na.omit(modeling_data$is_free))) else character()
  game_type_values <- if ("game_type" %in% names(modeling_data)) unique(as.character(stats::na.omit(modeling_data$game_type))) else character()
  category_indicators_vary <- length(category_columns) > 0L && all(vapply(
    category_columns,
    function(nm) length(unique(stats::na.omit(modeling_data[[nm]]))) > 1L,
    logical(1)
  ))

  target_props <- if ("is_free" %in% names(modeling_data)) {
    prop.table(table(modeling_data$is_free, useNA = "no"))
  } else {
    numeric()
  }

  rbind(
    validation_report_row(
      stage,
      "correct_column_names",
      if (length(missing_required) == 0L && has_expected_categories) "pass" else "fail",
      paste0("missing_required=", length(missing_required), "; cat_columns=", length(category_columns)),
      paste("required modeling columns and", expected_category_label),
      "modeling table has expected columns"
    ),
    validation_report_row(
      stage,
      "no_empty_observations",
      if (nrow(modeling_data) > 0L && all_missing_required_rows == 0L && blank_game_ids == 0L && blank_game_names == 0L) "pass" else "fail",
      paste0("rows=", nrow(modeling_data), "; all_missing_required_rows=", all_missing_required_rows, "; blank_game_ids=", blank_game_ids, "; blank_game_names=", blank_game_names),
      "rows > 0 and no empty id/name observations",
      "modeling table must contain rows and non-empty identifiers"
    ),
    validation_report_row(
      stage,
      "missingness_not_beyond_threshold",
      if (!is.na(max_predictor_missingness) && max_predictor_missingness == 0) "pass" else "fail",
      paste0("max_predictor_missingness=", format_validation_number(max_predictor_missingness)),
      "0 missing values in target and model predictors",
      "target and modeling predictors have no missing values"
    ),
    validation_report_row(
      stage,
      "optional_metadata_missingness",
      optional_status,
      format_validation_rates(optional_missingness),
      paste0("warn if > 0; fail if > ", format_validation_number(optional_metadata_fail_threshold)),
      "developer/publisher metadata is useful for reporting but is not used as a model predictor"
    ),
    validation_report_row(
      stage,
      "correct_data_types",
      if (length(type_mismatches) == 0L) "pass" else "fail",
      paste0(length(type_mismatches), " mismatches"),
      "all expected types match",
      if (length(type_mismatches) == 0L) "all checked columns match expected types" else paste("mismatches:", paste(type_mismatches, collapse = ", "))
    ),
    validation_report_row(
      stage,
      "no_duplicate_observations",
      if (full_row_duplicates == 0L && duplicate_game_ids == 0L) "pass" else "fail",
      paste0("full_row_duplicates=", full_row_duplicates, "; duplicate_game_ids=", duplicate_game_ids),
      "0 duplicates",
      "modeling observations must not repeat rows or game_id values"
    ),
    validation_report_row(
      stage,
      "no_outlier_or_anomalous_values",
      if (sum(required_age_bad, platform_count_bad, n_categories_bad, release_year_bad) == 0L) "pass" else "fail",
      paste0("required_age_bad=", required_age_bad, "; platform_count_bad=", platform_count_bad, "; n_categories_bad=", n_categories_bad, "; release_year_bad=", release_year_bad),
      paste0("required_age 0:18; platform_count 1:3; n_categories 0:30; release_year -1 or 2000:", current_year + 10),
      "modeled numeric fields must remain within expected Steam analysis ranges"
    ),
    validation_report_row(
      stage,
      "correct_category_levels",
      if (all(target_values %in% c("Free", "Paid")) && all(game_type_values %in% known_steam_game_types()) && category_indicators_vary) "pass" else "fail",
      paste0("target_values: ", paste(target_values, collapse = ", "), " game_type_values: ", paste(game_type_values, collapse = ", ")),
      "Free/Paid target; known Steam game_type; non-constant cat_ indicators",
      "categorical values match expected levels and category indicators vary"
    ),
    validation_report_row(
      stage,
      "target_distribution_expected",
      if (length(target_props) == 2L && min(target_props) >= min_class_prop) "pass" else "fail",
      format_validation_rates(target_props),
      paste0("both classes present and each >= ", format_validation_number(min_class_prop)),
      "modeled target distribution must contain enough examples from both classes"
    )
  )
}

build_training_correlation_validation_report <- function(
    train_data,
    predictors,
    target_col = "is_free",
    positive_class = "Free",
    target_warn_threshold = 0.80,
    target_fail_threshold = 0.95,
    feature_warn_threshold = 0.90,
    feature_fail_threshold = 0.98,
    stage = "training_split") {
  x_df <- training_feature_matrix(train_data, predictors)
  y <- as.numeric(as.character(train_data[[target_col]]) == positive_class)
  target_cors <- vapply(x_df, function(col) safe_abs_cor(col, y), numeric(1))
  max_target_cor <- if (length(target_cors) == 0L || all(is.na(target_cors))) 0 else max(target_cors, na.rm = TRUE)
  target_status <- if (max_target_cor >= target_fail_threshold) {
    "fail"
  } else if (max_target_cor >= target_warn_threshold) {
    "warn"
  } else {
    "pass"
  }

  feature_pairs <- top_feature_correlation_pairs(train_data, predictors, n = 5L)
  max_feature_cor <- if (nrow(feature_pairs) == 0L) 0 else max(feature_pairs$value, na.rm = TRUE)
  feature_status <- if (max_feature_cor >= feature_fail_threshold) {
    "fail"
  } else if (max_feature_cor >= feature_warn_threshold) {
    "warn"
  } else {
    "pass"
  }
  warned_feature_pairs <- feature_pairs[feature_pairs$value >= feature_warn_threshold, , drop = FALSE]
  warning_details <- if (nrow(warned_feature_pairs) == 0L) {
    "none"
  } else {
    paste0(warned_feature_pairs$feature_1, " ~ ", warned_feature_pairs$feature_2, collapse = ", ")
  }

  rbind(
    validation_report_row(
      stage,
      "no_anomalous_target_feature_correlations",
      target_status,
      paste0("max_abs_cor=", format_validation_number(max_target_cor)),
      paste0("warn >= ", format_validation_number(target_warn_threshold), "; fail >= ", format_validation_number(target_fail_threshold)),
      paste0("checked training split only; top correlations: ", format_top_named_correlations(target_cors))
    ),
    validation_report_row(
      stage,
      "no_anomalous_feature_feature_correlations",
      feature_status,
      paste0("max_abs_cor=", format_validation_number(max_feature_cor)),
      paste0("warn >= ", format_validation_number(feature_warn_threshold), "; fail >= ", format_validation_number(feature_fail_threshold)),
      paste0("checked training split only; top correlations: ", format_top_feature_correlation_pairs(feature_pairs), "; warnings: ", warning_details)
    )
  )
}

build_training_validation_report <- function(
    modeling_data,
    train_data,
    predictors,
    expected_category_count = NULL,
    stage = "model_input",
    training_stage = "training_split") {
  rbind(
    build_modeling_validation_report(
      modeling_data,
      stage = stage,
      expected_category_count = expected_category_count
    ),
    build_training_correlation_validation_report(
      train_data,
      predictors = predictors,
      stage = training_stage
    )
  )
}
