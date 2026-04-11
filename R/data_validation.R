

#' Read and validate .RDS dataframe
#'
#' @param path Path to an RDS file.
#' @param data_name Name used in report details.
#'
#' @return A dataframe
load_validated_rds_data_frame <- function(path, data_name = "data") {
  assert_single_string(path, "path")
  assert_single_string(data_name, "data_name")

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
    pointblank::col_vals_between(columns = "release_year", left = -1, right = current_year + 10) %>%
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
  validate_required_columns(train_data, predictors, "train_data")

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
  validate_required_columns(train_data, c(target_col, predictors), "train_data")

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
  validate_required_columns(train_data, predictors, "train_data")

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
      
      validate_required_columns(train_data, c(target_col, predictors), "train_data")

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

   
   