"
Train and evaluate the logistic regression model.

Usage:
  scripts/07_train-test-model.R <input_data_dir> <results_dir>

Options:
  <input_data_dir>  Directory containing wrangled_table.RDS from script 02.
  <results_dir>     Directory where model figures and CSV tables will be saved.
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
load_required_packages(c("docopt", "dplyr", "tibble", "ggplot2", "scales", "caret", "pROC", "pointblank"))
opt <- docopt::docopt(usage_doc)

source_project_file(project_root, "R", "io_validation_utils.R")
source_project_file(project_root, "R", "data_validation.R")
source_project_file(project_root, "R", "model_training.R")

invisible(run_train_test_model(
  input_data_dir = opt$input_data_dir,
  results_dir = opt$results_dir
))
