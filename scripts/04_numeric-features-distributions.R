"
Creates a faceted histogram grid for numeric predictors.

Usage: scripts/04_numeric-features-distributions.R <output_location_from_02> <output_to_location_04> <figure_storage_path>

Options:
<output_location_from_02> directory containing wrangled_table.RDS from script 02.
<output_to_location_04> directory where numeric_feature_distributions.RDS will be saved.
<figure_storage_path> directory where numeric_feature_distributions.png will be saved.
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
  input_data_dir = opt$output_location_from_02,
  output_object_dir = opt$output_to_location_04,
  output_figure_dir = opt$figure_storage_path,
  target_col = "is_free",
  predictors = c("required_age", "release_year", "platform_count", "n_categories"),
  drop_non_positive_for = "release_year"
)
