"
Creates a faceted histogram grid for numeric predictors using the wrangled data.

Usage: scripts/04_numeric-features-distributions.R <games_wrangled_data_save_location> <rds_save_location> <figures_storage_path>

Options:
<games_wrangled_data_save_location> location of the wrangled table from 02_data-preprocessing.R is stored.
<rds_save_location> location where numeric_feature_distributions.RDS will be saved.
<figures_storage_path> location where numeric_feature_distributions.png will be saved.
" -> usage_doc

if (!requireNamespace("docopt", quietly = TRUE)) {
  stop(
    paste(
      "Package `docopt` is required.",
      "Install it with:",
      "install.packages('docopt', repos = 'https://cloud.r-project.org')",
      "or run renv::restore() from the project root."
    ),
    call. = FALSE
  )
}

library(docopt)

find_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)

  if (length(script_arg) == 1L) {
    script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

opt <- docopt(usage_doc)

project_root <- find_project_root()
source(file.path(project_root, "R", "io_validation_utils.R"))
source(file.path(project_root, "R", "plot_numeric_distributions.R"))

run_numeric_features_distributions(
  input_data_dir = opt$games_wrangled_data_save_location,
  output_object_dir = opt$rds_save_location,
  output_figure_dir = opt$figures_storage_path,
  target_col = "is_free",
  predictors = c("required_age", "release_year", "platform_count", "n_categories"),
  drop_non_positive_for = "release_year"
)
