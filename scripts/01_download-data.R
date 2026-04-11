"
Download the Steam sample dataset and save it as games_sample.RDS.

Usage:
  scripts/01_download-data.R <input_url> <output_data_dir>

Options:
  <input_url>        HTTP(S) URL for the gzip-compressed Steam JSON dataset.
  <output_data_dir>  Directory where games_sample.RDS and games_sample.json are saved.
" -> usage_doc

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}
script_utils_path <- file.path(script_dir, "..", "R", "script_utils.R")
if (!file.exists(script_utils_path)) {
  script_utils_path <- file.path(getwd(), "R", "script_utils.R")
}
source(script_utils_path)

project_root <- find_project_root(script_dir)
load_required_packages(c("docopt", "jsonlite"))
opt <- docopt::docopt(usage_doc)

source_project_file(project_root, "R", "io_validation_utils.R")
source_project_file(project_root, "R", "download_data.R")

fallback_urls <- c(
  "https://media.githubusercontent.com/media/VintageDon/steam-dataset-2025/main/data/01_raw/steam_2025_5k-dataset-games_20250831.json.gz",
  "https://github.com/VintageDon/steam-dataset-2025/raw/main/data/01_raw/steam_2025_5k-dataset-games_20250831.json.gz",
  "https://raw.githubusercontent.com/VintageDon/steam-dataset-2025/main/data/01_raw/steam_2025_5k-dataset-games_20250831.json.gz"
)

invisible(run_data_download(
  input_url = opt$input_url,
  output_data_dir = opt$output_data_dir,
  fallback_urls = fallback_urls
))
