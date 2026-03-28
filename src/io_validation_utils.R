#' Assert that an argument is a single non-empty string
#'
#' @param value A value that should be a length-1 character string.
#' @param arg_name The argument name used in error messages.
#'
#' @return The validated string value.
#' @examples
#' assert_single_string("data", "path")
#'
assert_single_string <- function(value, arg_name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(sprintf("`%s` must be a single non-empty string.", arg_name), call. = FALSE)
  }
  value
}

#' Build a normalized path from a directory and filename
#'
#' @param directory A directory path.
#' @param filename A filename.
#'
#' @return A full path string.
#' @examples
#' build_file_path(tempdir(), "example.txt")
#'
build_file_path <- function(directory, filename) {
  assert_single_string(directory, "directory")
  assert_single_string(filename, "filename")
  file.path(directory, filename)
}

#' Ensure an output directory exists
#'
#' @param directory A directory path.
#' @param arg_name The argument name used in error messages.
#'
#' @return The input directory path.
#' @examples
#' ensure_directory_exists(tempdir(), "output_dir")
#'
ensure_directory_exists <- function(directory, arg_name) {
  assert_single_string(directory, arg_name)

  if (!dir.exists(directory)) {
    ok <- dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!ok || !dir.exists(directory)) {
      stop(sprintf("Could not create directory for `%s`: %s", arg_name, directory), call. = FALSE)
    }
  }

  directory
}

#' Read the wrangled modeling table from disk
#'
#' @param output_location_from_02 Directory containing `wrangled_table.RDS`.
#'
#' @return A data frame loaded from `wrangled_table.RDS`.
#' @examples
#' \dontrun{
#' load_wrangled_table("data/")
#' }
#'
load_wrangled_table <- function(output_location_from_02) {
  assert_single_string(output_location_from_02, "output_location_from_02")

  input_path <- build_file_path(output_location_from_02, "wrangled_table.RDS")
  if (!file.exists(input_path)) {
    stop(
      sprintf(
        "Input file not found: %s. Run script 02 first to create `wrangled_table.RDS`.",
        input_path
      ),
      call. = FALSE
    )
  }

  df_model <- readRDS(input_path)
  if (!is.data.frame(df_model)) {
    stop("`wrangled_table.RDS` must contain a data frame.", call. = FALSE)
  }

  df_model
}

#' Validate required columns in a data frame
#'
#' @param data A data frame.
#' @param required_columns Character vector of required column names.
#' @param data_name Name used in error messages.
#'
#' @return `TRUE` invisibly if validation passes.
#' @examples
#' validate_required_columns(data.frame(a = 1), "a", "df")
#'
validate_required_columns <- function(data, required_columns, data_name = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` must be a data frame.", data_name), call. = FALSE)
  }

  if (!is.character(required_columns) || length(required_columns) == 0L) {
    stop("`required_columns` must be a non-empty character vector.", call. = FALSE)
  }

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "`%s` is missing required columns: %s",
        data_name,
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
