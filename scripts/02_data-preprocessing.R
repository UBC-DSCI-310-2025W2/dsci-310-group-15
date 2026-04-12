"
Preprocess the downloaded Steam sample into the model-ready wrangled table.

Usage:
  scripts/02_data-preprocessing.R <input_data_dir> <output_data_dir> <table_output_dir>

Options:
  <input_data_dir>    Directory containing games_sample.RDS from script 01.
  <output_data_dir>   Directory where wrangled_table.RDS will be saved.
  <table_output_dir>  Directory where wrangled_table.csv will be saved.
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
load_required_packages(c("docopt", "dplyr", "tidyr", "purrr", "lubridate", "forcats", "janitor"))
opt <- docopt::docopt(usage_doc)

source_project_file(project_root, "R", "io_validation_utils.R")
source_project_file(project_root, "R", "extract_values.R")
source_project_file(project_root, "R", "preprocess_data.R")

invisible(run_data_preprocessing(
  input_data_dir = opt$input_data_dir,
  output_data_dir = opt$output_data_dir,
  table_output_dir = opt$table_output_dir,
  force = FALSE
))
