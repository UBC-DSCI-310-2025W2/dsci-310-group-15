"
Create category prevalence gap plots for free versus paid games.

Usage:
  scripts/06_categorical-features-plots.R <input_data_dir> <plot_object_dir> <figure_dir>

Options:
  <input_data_dir>   Directory containing wrangled_table.RDS from script 02.
  <plot_object_dir>  Directory where categorical_feat_gap.RDS will be saved.
  <figure_dir>       Directory where categorical_feat_gap.png will be saved.
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
load_required_packages(c("docopt", "dplyr", "tidyr", "ggplot2", "scales", "stringr"))
opt <- docopt::docopt(usage_doc)

source_project_file(project_root, "R", "io_validation_utils.R")
source_project_file(project_root, "R", "plot_categorical_features.R")

invisible(run_categorical_features_plot(
  input_data_dir = opt$input_data_dir,
  output_object_dir = opt$plot_object_dir,
  output_figure_dir = opt$figure_dir
))