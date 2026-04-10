"
Downloads the Steam 2025 5K Games dataset from GitHub and saves it as an RDS file.
The dataset URL is hardcoded in the script; no input file path is required.
Prints the number of rows and columns on success.

The dataset is sourced from:
  https://github.com/VintageDon/steam-dataset-2025

Usage: scripts/01_download-data.R <output_to_location_01>

Options:
<output_to_location_01>  Directory where games_sample.RDS will be saved.
                         Must be a relative path ending with a trailing slash.
                         Example: data/
" -> doc

# ---- Libraries ----
library(docopt)
required_packages <- c(
  "jsonlite", "tidyverse", "lubridate",
  "scales", "patchwork", "purrr", "janitor",  "knitr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Install missing packages before running:",
      paste(missing_packages, collapse = ", ")
    )
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

opt <- docopt(doc)

# ---- Reproducible data download ----
load_in <- function(output_to_location_01) {

  dataset_file <- "steam_2025_5k-dataset-games_20250831.json.gz"
  dataset_urls <- c(
    # Direct binary endpoint (works for Git LFS-backed files)
    "https://media.githubusercontent.com/media/VintageDon/steam-dataset-2025/main/data/01_raw/steam_2025_5k-dataset-games_20250831.json.gz",
    # Fallbacks
    "https://github.com/VintageDon/steam-dataset-2025/raw/main/data/01_raw/steam_2025_5k-dataset-games_20250831.json.gz",
    "https://raw.githubusercontent.com/VintageDon/steam-dataset-2025/main/data/01_raw/steam_2025_5k-dataset-games_20250831.json.gz"
  )

  project_root <- if (basename(getwd()) == "src") ".." else "."
  data_dir <- file.path(project_root, "data")
  raw_dir <- file.path(data_dir, "raw")

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  raw_path <- file.path(raw_dir, dataset_file)
  json_copy_path <- file.path(data_dir, "games_sample.json")

  is_lfs_pointer <- function(path) {
    if (!file.exists(path)) return(FALSE)

    first_line <- tryCatch(
      readLines(path, n = 1, warn = FALSE),
      error = function(e) character(0)
    )

    length(first_line) > 0 &&
      grepl("^version https://git-lfs.github.com/spec/v1", first_line)
  }

  is_gzip_file <- function(path) {
    if (!file.exists(path)) return(FALSE)

    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    sig <- tryCatch(readBin(con, what = "raw", n = 2), error = function(e) raw(0))

    length(sig) == 2 && identical(as.integer(sig), c(31L, 139L))
  }

  download_valid_dataset <- function(dest_path, urls) {
    for (u in urls) {
      ok <- suppressWarnings(
        tryCatch(
          {
            utils::download.file(u, destfile = dest_path, mode = "wb", quiet = TRUE)
            TRUE
          },
          error = function(e) FALSE
        )
      )

      if (!ok || !file.exists(dest_path)) next

      if (is_lfs_pointer(dest_path)) {
        unlink(dest_path)
        next
      }

      if (is_gzip_file(dest_path)) {
        return(u)
      }

      unlink(dest_path)
    }

    stop(
      paste0(
        "Could not download a valid gzip dataset file. Checked URLs:\n",
        paste(urls, collapse = "\n")
      )
    )
  }

  if (!file.exists(raw_path) || is_lfs_pointer(raw_path) || !is_gzip_file(raw_path)) {
    source_url <- download_valid_dataset(raw_path, dataset_urls)
  } else {
    source_url <- "existing_local_file"
  }

  needs_refresh <- !file.exists(json_copy_path) ||
    file.info(raw_path)$mtime > file.info(json_copy_path)$mtime ||
    is_lfs_pointer(json_copy_path)

  if (needs_refresh) {
    in_con <- gzfile(raw_path, open = "rb")
    out_con <- file(json_copy_path, open = "wb")

    tryCatch(
      {
        repeat {
          chunk <- readBin(in_con, what = "raw", n = 1024 * 1024)
          if (length(chunk) == 0) break
          writeBin(chunk, out_con)
        }
      },
      finally = {
        close(in_con)
        close(out_con)
      }
    )
  }

  if (is_lfs_pointer(json_copy_path)) {
    unlink(json_copy_path)
    stop("Local JSON copy is a Git LFS pointer, not real data. Re-run this cell.")
  }

  raw <- jsonlite::fromJSON(json_copy_path, flatten = TRUE)

  if (!"games" %in% names(raw)) {
    stop("Expected a top-level `games` field in the downloaded JSON.")
  }

  df <- raw$games

  cat("Dataset source:", source_url, "\n")
  cat("Raw compressed path:", normalizePath(raw_path, winslash = "/"), "\n")
  cat("Local JSON copy:", normalizePath(json_copy_path, winslash = "/"), "\n")
  cat("Rows:", nrow(df), "Columns:", ncol(df), "\n")

  saveRDS(df, paste(output_to_location_01, 'games_sample.RDS', sep = ''))
}

load_in(opt$output_to_location_01)