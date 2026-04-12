#' Check whether a file is a Git LFS pointer
#'
#' @param path Path to a candidate data file.
#'
#' @return `TRUE` if the file looks like a Git LFS pointer, otherwise `FALSE`.
is_lfs_pointer <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  first_line <- tryCatch(
    readLines(path, n = 1, warn = FALSE),
    error = function(e) character(0)
  )

  length(first_line) > 0L &&
    grepl("^version https://git-lfs.github.com/spec/v1", first_line)
}

#' Check whether a file starts with a gzip signature
#'
#' @param path Path to a candidate gzip file.
#'
#' @return `TRUE` if the file starts with the gzip magic bytes.
is_gzip_file <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  sig <- tryCatch(readBin(con, what = "raw", n = 2), error = function(e) raw(0))

  length(sig) == 2L && identical(as.integer(sig), c(31L, 139L))
}

#' Download the first valid gzip dataset from candidate URLs
#'
#' @param dest_path File path where the gzip file should be written.
#' @param urls Character vector of candidate URLs.
#'
#' @return The URL that successfully downloaded a valid gzip file.
download_valid_dataset <- function(dest_path, urls) {
  processandplot::assert_single_string(dest_path, "dest_path")

  if (!is.character(urls) || length(urls) == 0L || any(!nzchar(urls))) {
    stop("`urls` must be a non-empty character vector.", call. = FALSE)
  }

  for (url in urls) {
    ok <- suppressWarnings(
      tryCatch(
        {
          utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
          TRUE
        },
        error = function(e) FALSE
      )
    )

    if (!ok || !file.exists(dest_path)) {
      next
    }

    if (is_lfs_pointer(dest_path) || !is_gzip_file(dest_path)) {
      unlink(dest_path)
      next
    }

    return(url)
  }

  stop(
    paste0(
      "Could not download a valid gzip dataset file. Checked URLs:\n",
      paste(urls, collapse = "\n")
    ),
    call. = FALSE
  )
}

#' Decompress a gzip file to a JSON file
#'
#' @param gzip_path Path to a gzip-compressed JSON file.
#' @param json_path Path where the decompressed JSON should be written.
#'
#' @return Path to the decompressed JSON file.
decompress_gzip_to_json <- function(gzip_path, json_path) {
  processandplot::assert_single_string(gzip_path, "gzip_path")
  processandplot::assert_single_string(json_path, "json_path")

  in_con <- gzfile(gzip_path, open = "rb")
  out_con <- file(json_path, open = "wb")

  tryCatch(
    {
      repeat {
        chunk <- readBin(in_con, what = "raw", n = 1024 * 1024)
        if (length(chunk) == 0L) {
          break
        }
        writeBin(chunk, out_con)
      }
    },
    finally = {
      close(in_con)
      close(out_con)
    }
  )

  json_path
}

#' Download and serialize the Steam sample dataset
#'
#' Downloads the raw gzip JSON file, keeps a decompressed JSON copy, validates the
#' expected top-level `games` field, and saves the modeling source table as
#' `games_sample.RDS`.
#'
#' @param input_url Primary dataset URL.
#' @param output_data_dir Directory where `games_sample.RDS` and `games_sample.json`
#' should be saved.
#' @param raw_data_dir Directory where the downloaded gzip file should be cached.
#' @param dataset_filename Filename to use for the cached gzip file.
#' @param fallback_urls Optional fallback URLs to try after `input_url`.
#'
#' @return A named list containing output paths and row/column counts.
run_data_download <- function(
    input_url,
    output_data_dir,
    raw_data_dir = file.path(output_data_dir, "raw"),
    dataset_filename = "steam_2025_5k-dataset-games_20250831.json.gz",
    fallback_urls = character(0)) {
  processandplot::assert_single_string(input_url, "input_url")
  processandplot::assert_single_string(output_data_dir, "output_data_dir")
  processandplot::assert_single_string(raw_data_dir, "raw_data_dir")
  processandplot::assert_single_string(dataset_filename, "dataset_filename")

  if (!grepl("^https?://", input_url)) {
    stop("`input_url` must be an HTTP or HTTPS URL.", call. = FALSE)
  }

  processandplot::ensure_directory_exists(output_data_dir, "output_data_dir")
  processandplot::ensure_directory_exists(raw_data_dir, "raw_data_dir")

  raw_path <- processandplot::build_file_path(raw_data_dir, dataset_filename)
  json_copy_path <- processandplot::build_file_path(output_data_dir, "games_sample.json")
  rds_path <- processandplot::build_file_path(output_data_dir, "games_sample.RDS")
  dataset_urls <- unique(c(input_url, fallback_urls))

  if (!file.exists(raw_path) || is_lfs_pointer(raw_path) || !is_gzip_file(raw_path)) {
    source_url <- download_valid_dataset(raw_path, dataset_urls)
  } else {
    source_url <- "existing_local_file"
  }

  needs_refresh <- !file.exists(json_copy_path) ||
    file.info(raw_path)$mtime > file.info(json_copy_path)$mtime ||
    is_lfs_pointer(json_copy_path)

  if (needs_refresh) {
    decompress_gzip_to_json(raw_path, json_copy_path)
  }

  if (is_lfs_pointer(json_copy_path)) {
    unlink(json_copy_path)
    stop("Local JSON copy is a Git LFS pointer, not real data.", call. = FALSE)
  }

  needs_rds_refresh <- !file.exists(rds_path) ||
    file.info(json_copy_path)$mtime > file.info(rds_path)$mtime ||
    is_lfs_pointer(rds_path)

  if (needs_rds_refresh) {
    raw <- jsonlite::fromJSON(json_copy_path, flatten = TRUE)

    if (!"games" %in% names(raw)) {
      stop("Expected a top-level `games` field in the downloaded JSON.", call. = FALSE)
    }

    games <- raw$games
    saveRDS(games, rds_path)
  } else {
    games <- readRDS(rds_path)
  }

  message("Dataset source: ", source_url)
  message("Raw compressed path: ", normalizePath(raw_path, winslash = "/"))
  message("Local JSON copy: ", normalizePath(json_copy_path, winslash = "/"))
  message("Rows: ", nrow(games), " Columns: ", ncol(games))

  list(
    source_url = source_url,
    raw_path = raw_path,
    json_path = json_copy_path,
    rds_path = rds_path,
    rows = nrow(games),
    columns = ncol(games)
  )
}
