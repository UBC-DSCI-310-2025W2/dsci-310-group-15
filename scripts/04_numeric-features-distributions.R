"
Create a faceted histogram grid for numeric predictors.

Usage:
  scripts/04_numeric-features-distributions.R <input_data_dir> <plot_object_dir> <figure_dir>

Options:
  <input_data_dir>   Directory containing wrangled_table.RDS from script 02.
  <plot_object_dir>  Directory where numeric_feature_distributions.RDS will be saved.
  <figure_dir>       Directory where numeric_feature_distributions.png will be saved.
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
load_required_packages(c("docopt", "dplyr", "tidyr", "ggplot2", "scales"))
opt <- docopt::docopt(usage_doc)

source_project_file(project_root, "R", "io_validation_utils.R")
source_project_file(project_root, "R", "plot_numeric_distributions.R")

invisible(run_numeric_features_distributions(
  input_data_dir = opt$input_data_dir,
  output_object_dir = opt$plot_object_dir,
  output_figure_dir = opt$figure_dir,
  target_col = "is_free",
  predictors = c("required_age", "release_year", "platform_count", "n_categories"),
  drop_non_positive_for = "release_year"
))
