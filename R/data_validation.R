#' Build one validation report row
#'
#' @param stage Pipeline stage being validated.
#' @param check Check name.
#' @param status One of `pass`, `warn`, or `fail`.
#' @param observed_value Observed value to report.
#' @param threshold Threshold or expectation used by the check.
#' @param details Human-readable details.
#'
#' @return A one-row data frame.
validation_result <- function(
    stage,
    check,
    status,
    observed_value = NA_character_,
    threshold = NA_character_,
    details = "") {
  assert_single_string(stage, "stage")
  assert_single_string(check, "check")
  assert_single_string(status, "status")

  if (!status %in% c("pass", "warn", "fail")) {
    stop("`status` must be one of `pass`, `warn`, or `fail`.", call. = FALSE)
  }

  data.frame(
    stage = stage,
    check = check,
    status = status,
    observed_value = collapse_validation_value(observed_value),
    threshold = collapse_validation_value(threshold),
    details = collapse_validation_value(details),
    stringsAsFactors = FALSE
  )
}

#' Collapse a validation value to one printable string
#'
#' @param value Value to collapse.
#'
#' @return A length-1 character vector.
collapse_validation_value <- function(value) {
  if (length(value) == 0L) {
    return(NA_character_)
  }

  if (all(is.na(value))) {
    return(NA_character_)
  }

  paste(as.character(value), collapse = "; ")
}

#' Combine validation report fragments
#'
#' @param ... Validation report data frames.
#'
#' @return A combined validation report data frame.
combine_validation_reports <- function(...) {
  reports <- Filter(Negate(is.null), list(...))

  if (length(reports) == 0L) {
    return(data.frame(
      stage = character(),
      check = character(),
      status = character(),
      observed_value = character(),
      threshold = character(),
      details = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, reports)
}

#' Save a validation report as CSV
#'
#' @param report Validation report data frame.
#' @param output_dir Output directory.
#' @param filename Output CSV filename.
#'
#' @return Path to the written report.
save_validation_report <- function(
    report,
    output_dir,
    filename = "data_validation_report.csv") {
  if (!is.data.frame(report)) {
    stop("`report` must be a data frame.", call. = FALSE)
  }

  ensure_directory_exists(output_dir, "output_dir")
  assert_single_string(filename, "filename")

  output_path <- build_file_path(output_dir, filename)
  utils::write.csv(report, output_path, row.names = FALSE, na = "")
  output_path
}

#' Stop when a validation report contains failures
#'
#' @param report Validation report data frame.
#'
#' @return Invisibly returns `TRUE` when no failures are present.
assert_no_validation_failures <- function(report) {
  validate_required_columns(
    report,
    c("stage", "check", "status", "details"),
    "report"
  )

  failures <- report[report$status == "fail", , drop = FALSE]
  if (nrow(failures) > 0L) {
    failure_messages <- paste(
      failures$stage,
      failures$check,
      failures$details,
      sep = " - "
    )
    stop(
      paste(
        "Data validation failed:",
        paste(failure_messages, collapse = " | ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Load an RDS file and validate that it contains a data frame
#'
#' @param path Path to an RDS file.
#' @param stage Pipeline stage label.
#' @param data_name Name used in report details.
#'
#' @return A list with `data` and `report`.
load_validated_rds_data_frame <- function(path, stage, data_name = "data") {
  assert_single_string(path, "path")
  assert_single_string(stage, "stage")
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
  status <- if (has_rds_extension && path_exists && is.null(load_error) && is_valid_data_frame) {
    "pass"
  } else {
    "fail"
  }

  details <- c(
    if (!has_rds_extension) "expected .RDS extension" else "extension ok",
    if (!path_exists) "file missing" else "file exists",
    if (!is.null(load_error)) paste("readRDS error:", load_error) else "readRDS ok",
    if (!is_valid_data_frame) paste(data_name, "must be a data frame") else "data frame ok"
  )

  list(
    data = loaded_data,
    report = validation_result(
      stage = stage,
      check = "correct_data_file_format",
      status = status,
      observed_value = path,
      threshold = ".RDS data frame",
      details = paste(details, collapse = "; ")
    )
  )
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

#' Check whether a column matches an expected type
#'
#' @param column Data frame column.
#' @param expected Expected type label.
#'
#' @return `TRUE` if the column matches.
column_matches_expected_type <- function(column, expected) {
  switch(
    expected,
    character = is.character(column),
    factor = is.factor(column),
    integer = is.integer(column),
    integerish = is.integer(column) ||
      (is.numeric(column) && all(is.na(column) | column == floor(column))),
    logical = is.logical(column),
    numeric = is.numeric(column) || is.integer(column),
    list = is.list(column) && !is.data.frame(column),
    stop(sprintf("Unsupported expected type: %s", expected), call. = FALSE)
  )
}

#' Validate a named vector of expected column types
#'
#' @param data Data frame to validate.
#' @param expected_types Named character vector of expected types.
#' @param stage Pipeline stage label.
#'
#' @return A validation report row.
validate_column_types_report <- function(data, expected_types, stage) {
  missing_columns <- setdiff(names(expected_types), names(data))
  present_columns <- intersect(names(expected_types), names(data))

  mismatches <- character(0)
  for (column_name in present_columns) {
    expected_type <- expected_types[[column_name]]
    if (!column_matches_expected_type(data[[column_name]], expected_type)) {
      mismatches <- c(
        mismatches,
        sprintf(
          "%s expected %s got %s",
          column_name,
          expected_type,
          paste(class(data[[column_name]]), collapse = "/")
        )
      )
    }
  }

  status <- if (length(missing_columns) > 0L || length(mismatches) > 0L) {
    "fail"
  } else {
    "pass"
  }

  validation_result(
    stage = stage,
    check = "correct_data_types",
    status = status,
    observed_value = sprintf("%d mismatches", length(mismatches)),
    threshold = "all expected types match",
    details = paste(c(
      if (length(missing_columns) > 0L) {
        paste("missing columns:", paste(missing_columns, collapse = ", "))
      },
      mismatches,
      if (length(missing_columns) == 0L && length(mismatches) == 0L) {
        "all checked columns match expected types"
      }
    ), collapse = "; ")
  )
}

#' Compute missing flags for atomic or list columns
#'
#' @param column Data frame column.
#'
#' @return Logical vector of missing flags.
column_missing_flags <- function(column) {
  if (is.list(column) && !is.data.frame(column)) {
    return(vapply(
      column,
      function(value) {
        is.null(value) ||
          length(value) == 0L ||
          (length(value) == 1L && is.atomic(value) && is.na(value))
      },
      logical(1)
    ))
  }

  is.na(column)
}

#' Compute per-column missing proportions
#'
#' @param data Data frame.
#' @param columns Columns to summarize.
#'
#' @return Named numeric vector of missing proportions.
missing_proportions <- function(data, columns) {
  stats::setNames(
    vapply(
      columns,
      function(column_name) mean(column_missing_flags(data[[column_name]])),
      numeric(1)
    ),
    columns
  )
}

#' Format top named numeric values
#'
#' @param values Named numeric vector.
#' @param n Number of values to include.
#'
#' @return Printable string.
format_top_named_values <- function(values, n = 5L) {
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    return("none")
  }

  values <- sort(values, decreasing = TRUE)
  values <- utils::head(values, n)
  paste(sprintf("%s=%.3f", names(values), values), collapse = ", ")
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
    return(validation_result(
      stage,
      "correct_data_file_format",
      "fail",
      paste(class(games_data), collapse = "/"),
      "data frame",
      "`games_data` must be a data frame"
    ))
  }

  required_columns <- raw_steam_required_columns()
  missing_columns <- setdiff(required_columns, names(games_data))
  present_required <- intersect(required_columns, names(games_data))

  column_report <- validation_result(
    stage = stage,
    check = "correct_column_names",
    status = if (length(missing_columns) == 0L) "pass" else "fail",
    observed_value = sprintf("%d missing", length(missing_columns)),
    threshold = "all raw modeling columns present",
    details = if (length(missing_columns) == 0L) {
      "all required raw columns are present"
    } else {
      paste("missing columns:", paste(missing_columns, collapse = ", "))
    }
  )

  row_missing_count <- if (length(present_required) == 0L) {
    NA_integer_
  } else {
    missing_matrix <- vapply(
      present_required,
      function(column_name) column_missing_flags(games_data[[column_name]]),
      logical(nrow(games_data))
    )
    sum(rowSums(missing_matrix) == length(present_required))
  }
  blank_appid_count <- if ("appid" %in% names(games_data)) {
    sum(column_missing_flags(games_data$appid))
  } else {
    NA_integer_
  }
  blank_name_count <- if ("name_from_applist" %in% names(games_data)) {
    sum(is.na(games_data$name_from_applist) | !nzchar(trimws(as.character(games_data$name_from_applist))))
  } else {
    NA_integer_
  }

  empty_report <- validation_result(
    stage = stage,
    check = "no_empty_observations",
    status = if (
      nrow(games_data) > 0L &&
        identical(row_missing_count, 0L) &&
        identical(blank_appid_count, 0L) &&
        identical(blank_name_count, 0L)
    ) "pass" else "fail",
    observed_value = sprintf(
      "rows=%d; all_missing_required_rows=%s; blank_appids=%s; blank_names=%s",
      nrow(games_data),
      row_missing_count,
      blank_appid_count,
      blank_name_count
    ),
    threshold = "rows > 0 and no empty id/name observations",
    details = "raw table must contain rows and non-empty required identifiers"
  )

  app_detail_columns <- grep("^app_details\\.data\\.", present_required, value = TRUE)
  app_detail_columns <- app_detail_columns[
    !vapply(games_data[app_detail_columns], is.list, logical(1))
  ]
  raw_missingness <- if (length(app_detail_columns) > 0L) {
    missing_proportions(games_data, app_detail_columns)
  } else {
    stats::setNames(numeric(0), character(0))
  }
  high_missingness <- names(raw_missingness)[raw_missingness > missing_threshold]

  missingness_report <- validation_result(
    stage = stage,
    check = "missingness_not_beyond_threshold",
    status = if (length(high_missingness) == 0L && length(app_detail_columns) > 0L) "pass" else "fail",
    observed_value = if (length(raw_missingness) > 0L) {
      sprintf("max=%.3f", max(raw_missingness))
    } else {
      "not_computed"
    },
    threshold = sprintf("<= %.3f for required app-detail fields", missing_threshold),
    details = if (length(high_missingness) == 0L && length(app_detail_columns) > 0L) {
      paste("highest missingness:", format_top_named_values(raw_missingness))
    } else {
      paste("columns above threshold:", paste(high_missingness, collapse = ", "))
    }
  )

  raw_expected_types <- c(
    appid = "integerish",
    name_from_applist = "character",
    "app_details.data.required_age" = "character",
    "app_details.data.is_free" = "logical",
    "app_details.data.type" = "character",
    "app_details.data.release_date.date" = "character",
    "app_details.data.platforms.windows" = "logical",
    "app_details.data.platforms.mac" = "logical",
    "app_details.data.platforms.linux" = "logical",
    "app_details.data.categories" = "list",
    "app_details.data.dlc" = "list",
    "app_details.data.demos" = "list",
    "app_details.data.developers" = "list",
    "app_details.data.publishers" = "list"
  )
  type_report <- validate_column_types_report(games_data, raw_expected_types, stage)

  full_duplicate_count <- tryCatch(
    sum(duplicated(games_data)),
    error = function(error) NA_integer_
  )
  duplicate_id_count <- if ("appid" %in% names(games_data)) {
    sum(duplicated(games_data$appid))
  } else {
    NA_integer_
  }
  duplicate_report <- validation_result(
    stage = stage,
    check = "no_duplicate_observations",
    status = if (
      (is.na(full_duplicate_count) || full_duplicate_count == 0L) &&
        identical(duplicate_id_count, 0L)
    ) "pass" else "fail",
    observed_value = sprintf(
      "full_row_duplicates=%s; duplicate_appids=%s",
      full_duplicate_count,
      duplicate_id_count
    ),
    threshold = "0 duplicates",
    details = "raw observations must not repeat appid values"
  )

  target_values <- if ("app_details.data.is_free" %in% names(games_data)) {
    unique(stats::na.omit(as.character(games_data[["app_details.data.is_free"]])))
  } else {
    character(0)
  }
  game_type_values <- if ("app_details.data.type" %in% names(games_data)) {
    unique(stats::na.omit(as.character(games_data[["app_details.data.type"]])))
  } else {
    character(0)
  }
  bad_target_values <- setdiff(target_values, c("TRUE", "FALSE"))
  bad_game_type_values <- setdiff(game_type_values, known_steam_game_types())
  category_report <- validation_result(
    stage = stage,
    check = "correct_category_levels",
    status = if (
      length(bad_target_values) == 0L &&
        length(bad_game_type_values) == 0L &&
        length(target_values) == 2L
    ) "pass" else "fail",
    observed_value = paste(
      "target_values:",
      paste(target_values, collapse = ", "),
      "game_type_values:",
      paste(game_type_values, collapse = ", ")
    ),
    threshold = "target TRUE/FALSE; known Steam type values",
    details = paste(c(
      if (length(bad_target_values) > 0L) {
        paste("unexpected target values:", paste(bad_target_values, collapse = ", "))
      },
      if (length(bad_game_type_values) > 0L) {
        paste("unexpected game_type values:", paste(bad_game_type_values, collapse = ", "))
      },
      if (length(target_values) != 2L) "target must have both TRUE and FALSE values",
      if (
        length(bad_target_values) == 0L &&
          length(bad_game_type_values) == 0L &&
          length(target_values) == 2L
      ) "raw categorical values match expected levels"
    ), collapse = "; ")
  )

  target_counts <- if ("app_details.data.is_free" %in% names(games_data)) {
    table(games_data[["app_details.data.is_free"]], useNA = "no")
  } else {
    integer(0)
  }
  target_props <- if (sum(target_counts) > 0L) target_counts / sum(target_counts) else numeric(0)
  target_report <- validation_result(
    stage = stage,
    check = "target_distribution_expected",
    status = if (length(target_props) == 2L && min(target_props) >= min_class_prop) "pass" else "fail",
    observed_value = if (length(target_props) > 0L) {
      paste(sprintf("%s=%.3f", names(target_props), target_props), collapse = ", ")
    } else {
      "no target values"
    },
    threshold = sprintf("both classes present and each >= %.3f", min_class_prop),
    details = "raw target distribution must contain both free and paid classes"
  )

  combine_validation_reports(
    column_report,
    empty_report,
    missingness_report,
    type_report,
    duplicate_report,
    category_report,
    target_report
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
    return(validation_result(
      stage,
      "correct_data_file_format",
      "fail",
      paste(class(modeling_data), collapse = "/"),
      "data frame",
      "`modeling_data` must be a data frame"
    ))
  }

  required_columns <- modeling_required_columns()
  category_columns <- grep("^cat_", names(modeling_data), value = TRUE)
  missing_columns <- setdiff(required_columns, names(modeling_data))
  column_failures <- c(
    if (length(missing_columns) > 0L) {
      paste("missing columns:", paste(missing_columns, collapse = ", "))
    },
    if (length(category_columns) == 0L) "no cat_ indicator columns found",
    if (!is.null(expected_category_count) && length(category_columns) != as.integer(expected_category_count)) {
      sprintf("expected %d cat_ columns but found %d", as.integer(expected_category_count), length(category_columns))
    }
  )
  column_report <- validation_result(
    stage = stage,
    check = "correct_column_names",
    status = if (length(column_failures) == 0L) "pass" else "fail",
    observed_value = sprintf(
      "missing_required=%d; cat_columns=%d",
      length(missing_columns),
      length(category_columns)
    ),
    threshold = if (is.null(expected_category_count)) {
      "required modeling columns and at least one cat_ column"
    } else {
      sprintf("required modeling columns and %d cat_ columns", as.integer(expected_category_count))
    },
    details = if (length(column_failures) == 0L) {
      "modeling table has expected columns"
    } else {
      paste(column_failures, collapse = "; ")
    }
  )

  present_required <- intersect(required_columns, names(modeling_data))
  row_missing_count <- if (length(present_required) == 0L) {
    NA_integer_
  } else {
    missing_matrix <- vapply(
      present_required,
      function(column_name) column_missing_flags(modeling_data[[column_name]]),
      logical(nrow(modeling_data))
    )
    sum(rowSums(missing_matrix) == length(present_required))
  }
  blank_game_id_count <- if ("game_id" %in% names(modeling_data)) {
    sum(column_missing_flags(modeling_data$game_id))
  } else {
    NA_integer_
  }
  blank_game_name_count <- if ("game_name" %in% names(modeling_data)) {
    sum(is.na(modeling_data$game_name) | !nzchar(trimws(as.character(modeling_data$game_name))))
  } else {
    NA_integer_
  }
  empty_report <- validation_result(
    stage = stage,
    check = "no_empty_observations",
    status = if (
      nrow(modeling_data) > 0L &&
        identical(row_missing_count, 0L) &&
        identical(blank_game_id_count, 0L) &&
        identical(blank_game_name_count, 0L)
    ) "pass" else "fail",
    observed_value = sprintf(
      "rows=%d; all_missing_required_rows=%s; blank_game_ids=%s; blank_game_names=%s",
      nrow(modeling_data),
      row_missing_count,
      blank_game_id_count,
      blank_game_name_count
    ),
    threshold = "rows > 0 and no empty id/name observations",
    details = "modeling table must contain rows and non-empty identifiers"
  )

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
  present_predictor_columns <- intersect(predictor_columns, names(modeling_data))
  predictor_missingness <- if (length(present_predictor_columns) > 0L) {
    missing_proportions(modeling_data, present_predictor_columns)
  } else {
    stats::setNames(numeric(0), character(0))
  }
  missing_predictors <- names(predictor_missingness)[predictor_missingness > 0]
  missingness_report <- validation_result(
    stage = stage,
    check = "missingness_not_beyond_threshold",
    status = if (length(missing_predictors) == 0L && length(predictor_missingness) > 0L) "pass" else "fail",
    observed_value = if (length(predictor_missingness) > 0L) {
      sprintf("max_predictor_missingness=%.3f", max(predictor_missingness))
    } else {
      "not_computed"
    },
    threshold = "0 missing values in target and model predictors",
    details = if (length(missing_predictors) == 0L && length(predictor_missingness) > 0L) {
      "target and modeling predictors have no missing values"
    } else {
      paste("predictor columns with missing values:", paste(missing_predictors, collapse = ", "))
    }
  )

  optional_columns <- intersect(c("developer_name", "publisher_name"), names(modeling_data))
  optional_missingness <- if (length(optional_columns) > 0L) {
    missing_proportions(modeling_data, optional_columns)
  } else {
    stats::setNames(numeric(0), character(0))
  }
  optional_max <- if (length(optional_missingness) > 0L) max(optional_missingness) else NA_real_
  optional_report <- validation_result(
    stage = stage,
    check = "optional_metadata_missingness",
    status = if (length(optional_missingness) == 0L || is.na(optional_max)) {
      "fail"
    } else if (optional_max > optional_metadata_fail_threshold) {
      "fail"
    } else if (optional_max > 0) {
      "warn"
    } else {
      "pass"
    },
    observed_value = if (length(optional_missingness) > 0L) {
      format_top_named_values(optional_missingness, n = length(optional_missingness))
    } else {
      "not_computed"
    },
    threshold = sprintf("warn if > 0; fail if > %.3f", optional_metadata_fail_threshold),
    details = "developer/publisher metadata is useful for reporting but is not used as a model predictor"
  )

  expected_types <- c(
    game_id = "integerish",
    game_name = "character",
    is_free = "factor",
    required_age = "integerish",
    release_year = "numeric",
    game_type = "factor",
    windows_support = "logical",
    mac_support = "logical",
    linux_support = "logical",
    platform_count = "integerish",
    has_dlc = "logical",
    has_demo = "logical",
    n_categories = "integerish",
    developer_name = "character",
    publisher_name = "character",
    stats::setNames(rep("logical", length(category_columns)), category_columns)
  )
  type_report <- validate_column_types_report(modeling_data, expected_types, stage)

  full_duplicate_count <- tryCatch(
    sum(duplicated(modeling_data)),
    error = function(error) NA_integer_
  )
  duplicate_id_count <- if ("game_id" %in% names(modeling_data)) {
    sum(duplicated(modeling_data$game_id))
  } else {
    NA_integer_
  }
  duplicate_report <- validation_result(
    stage = stage,
    check = "no_duplicate_observations",
    status = if (identical(full_duplicate_count, 0L) && identical(duplicate_id_count, 0L)) {
      "pass"
    } else {
      "fail"
    },
    observed_value = sprintf(
      "full_row_duplicates=%s; duplicate_game_ids=%s",
      full_duplicate_count,
      duplicate_id_count
    ),
    threshold = "0 duplicates",
    details = "modeling observations must not repeat rows or game_id values"
  )

  required_age_bad <- if ("required_age" %in% names(modeling_data)) {
    sum(!is.na(modeling_data$required_age) & !(modeling_data$required_age %in% 0:18))
  } else {
    NA_integer_
  }
  platform_count_bad <- if ("platform_count" %in% names(modeling_data)) {
    sum(!is.na(modeling_data$platform_count) & !(modeling_data$platform_count %in% 1:3))
  } else {
    NA_integer_
  }
  n_categories_bad <- if ("n_categories" %in% names(modeling_data)) {
    sum(!is.na(modeling_data$n_categories) & (modeling_data$n_categories < 0 | modeling_data$n_categories > 30))
  } else {
    NA_integer_
  }
  release_year_bad <- if ("release_year" %in% names(modeling_data)) {
    sum(!is.na(modeling_data$release_year) &
      !(modeling_data$release_year == -1 |
        (modeling_data$release_year >= 2000 & modeling_data$release_year <= current_year + 10)))
  } else {
    NA_integer_
  }
  range_counts <- c(
    required_age = required_age_bad,
    platform_count = platform_count_bad,
    n_categories = n_categories_bad,
    release_year = release_year_bad
  )
  range_report <- validation_result(
    stage = stage,
    check = "no_outlier_or_anomalous_values",
    status = if (all(!is.na(range_counts)) && all(range_counts == 0L)) "pass" else "fail",
    observed_value = paste(sprintf("%s_bad=%s", names(range_counts), range_counts), collapse = "; "),
    threshold = sprintf("required_age 0:18; platform_count 1:3; n_categories 0:30; release_year -1 or 2000:%d", current_year + 10),
    details = "modeled numeric fields must remain within expected Steam analysis ranges"
  )

  target_values <- if ("is_free" %in% names(modeling_data)) {
    unique(stats::na.omit(as.character(modeling_data$is_free)))
  } else {
    character(0)
  }
  target_levels <- if ("is_free" %in% names(modeling_data) && is.factor(modeling_data$is_free)) {
    levels(modeling_data$is_free)
  } else {
    character(0)
  }
  game_type_values <- if ("game_type" %in% names(modeling_data)) {
    unique(stats::na.omit(as.character(modeling_data$game_type)))
  } else {
    character(0)
  }
  bad_target_values <- setdiff(target_values, c("Free", "Paid"))
  bad_target_levels <- setdiff(target_levels, c("Free", "Paid"))
  bad_game_type_values <- setdiff(game_type_values, known_steam_game_types())
  constant_category_columns <- category_columns[
    vapply(
      category_columns,
      function(column_name) length(unique(stats::na.omit(modeling_data[[column_name]]))) < 2L,
      logical(1)
    )
  ]
  category_level_failures <- c(
    if (length(bad_target_values) > 0L) {
      paste("unexpected target values:", paste(bad_target_values, collapse = ", "))
    },
    if (!setequal(target_levels, c("Free", "Paid"))) {
      paste("target factor levels must be exactly Free/Paid; found:", paste(target_levels, collapse = ", "))
    },
    if (length(target_values) != 2L) "target must have both Free and Paid values",
    if (length(bad_target_levels) > 0L) {
      paste("unexpected target levels:", paste(bad_target_levels, collapse = ", "))
    },
    if (length(bad_game_type_values) > 0L) {
      paste("unexpected game_type values:", paste(bad_game_type_values, collapse = ", "))
    },
    if (length(constant_category_columns) > 0L) {
      paste("constant cat_ columns:", paste(constant_category_columns, collapse = ", "))
    }
  )
  category_report <- validation_result(
    stage = stage,
    check = "correct_category_levels",
    status = if (length(category_level_failures) == 0L) "pass" else "fail",
    observed_value = paste(
      "target_values:",
      paste(target_values, collapse = ", "),
      "game_type_values:",
      paste(game_type_values, collapse = ", ")
    ),
    threshold = "Free/Paid target; known Steam game_type; non-constant cat_ indicators",
    details = if (length(category_level_failures) == 0L) {
      "categorical values match expected levels and category indicators vary"
    } else {
      paste(category_level_failures, collapse = "; ")
    }
  )

  target_counts <- if ("is_free" %in% names(modeling_data)) {
    table(modeling_data$is_free, useNA = "no")
  } else {
    integer(0)
  }
  target_props <- if (sum(target_counts) > 0L) target_counts / sum(target_counts) else numeric(0)
  target_report <- validation_result(
    stage = stage,
    check = "target_distribution_expected",
    status = if (length(target_props) == 2L && min(target_props) >= min_class_prop) "pass" else "fail",
    observed_value = if (length(target_props) > 0L) {
      paste(sprintf("%s=%.3f", names(target_props), target_props), collapse = ", ")
    } else {
      "no target values"
    },
    threshold = sprintf("both classes present and each >= %.3f", min_class_prop),
    details = "modeled target distribution must contain enough examples from both classes"
  )

  combine_validation_reports(
    column_report,
    empty_report,
    missingness_report,
    optional_report,
    type_report,
    duplicate_report,
    range_report,
    category_report,
    target_report
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
  validate_required_columns(train_data, target_col, "train_data")

  if (!is.character(predictors) || length(predictors) == 0L) {
    stop("`predictors` must be a non-empty character vector.", call. = FALSE)
  }
  validate_required_columns(train_data, predictors, "train_data")

  y <- as.numeric(as.character(train_data[[target_col]]) == positive_class)
  numeric_predictors <- predictors[
    vapply(
      predictors,
      function(column_name) {
        is.numeric(train_data[[column_name]]) ||
          is.integer(train_data[[column_name]]) ||
          is.logical(train_data[[column_name]])
      },
      logical(1)
    )
  ]

  target_correlations <- stats::setNames(
    vapply(
      numeric_predictors,
      function(column_name) safe_abs_cor(as.numeric(train_data[[column_name]]), y),
      numeric(1)
    ),
    numeric_predictors
  )

  target_failures <- names(target_correlations)[
    !is.na(target_correlations) & target_correlations >= target_fail_threshold
  ]
  target_warnings <- names(target_correlations)[
    !is.na(target_correlations) &
      target_correlations >= target_warn_threshold &
      target_correlations < target_fail_threshold
  ]
  target_report <- validation_result(
    stage = stage,
    check = "no_anomalous_target_feature_correlations",
    status = if (length(target_failures) > 0L) {
      "fail"
    } else if (length(target_warnings) > 0L) {
      "warn"
    } else {
      "pass"
    },
    observed_value = if (length(target_correlations) > 0L && any(!is.na(target_correlations))) {
      sprintf("max_abs_cor=%.3f", max(target_correlations, na.rm = TRUE))
    } else {
      "not_computed"
    },
    threshold = sprintf("warn >= %.2f; fail >= %.2f", target_warn_threshold, target_fail_threshold),
    details = paste(c(
      "checked training split only",
      paste("top correlations:", format_top_named_values(target_correlations)),
      if (length(target_failures) > 0L) {
        paste("failures:", paste(target_failures, collapse = ", "))
      },
      if (length(target_warnings) > 0L) {
        paste("warnings:", paste(target_warnings, collapse = ", "))
      }
    ), collapse = "; ")
  )

  feature_pair_values <- stats::setNames(numeric(0), character(0))
  if (length(numeric_predictors) >= 2L) {
    numeric_data <- as.data.frame(
      lapply(train_data[numeric_predictors], as.numeric),
      stringsAsFactors = FALSE
    )
    correlation_matrix <- suppressWarnings(abs(stats::cor(numeric_data, use = "pairwise.complete.obs")))
    correlation_matrix[lower.tri(correlation_matrix, diag = TRUE)] <- NA_real_
    pair_indices <- which(!is.na(correlation_matrix), arr.ind = TRUE)
    if (nrow(pair_indices) > 0L) {
      feature_pair_values <- stats::setNames(
        correlation_matrix[pair_indices],
        paste(
          rownames(correlation_matrix)[pair_indices[, "row"]],
          colnames(correlation_matrix)[pair_indices[, "col"]],
          sep = " ~ "
        )
      )
    }
  }

  feature_failures <- names(feature_pair_values)[
    !is.na(feature_pair_values) & feature_pair_values >= feature_fail_threshold
  ]
  feature_warnings <- names(feature_pair_values)[
    !is.na(feature_pair_values) &
      feature_pair_values >= feature_warn_threshold &
      feature_pair_values < feature_fail_threshold
  ]
  feature_report <- validation_result(
    stage = stage,
    check = "no_anomalous_feature_feature_correlations",
    status = if (length(feature_failures) > 0L) {
      "fail"
    } else if (length(feature_warnings) > 0L) {
      "warn"
    } else {
      "pass"
    },
    observed_value = if (length(feature_pair_values) > 0L && any(!is.na(feature_pair_values))) {
      sprintf("max_abs_cor=%.3f", max(feature_pair_values, na.rm = TRUE))
    } else {
      "not_computed"
    },
    threshold = sprintf("warn >= %.2f; fail >= %.2f", feature_warn_threshold, feature_fail_threshold),
    details = paste(c(
      "checked training split only",
      paste("top correlations:", format_top_named_values(feature_pair_values)),
      if (length(feature_failures) > 0L) {
        paste("failures:", paste(feature_failures, collapse = ", "))
      },
      if (length(feature_warnings) > 0L) {
        paste("warnings:", paste(feature_warnings, collapse = ", "))
      }
    ), collapse = "; ")
  )

  combine_validation_reports(target_report, feature_report)
}
