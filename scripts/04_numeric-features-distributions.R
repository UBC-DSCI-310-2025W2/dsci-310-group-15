"
Creates a faceted histogram grid for numeric predictors.

Usage: scripts/04_numeric-features-distributions.R <output_location_from_02> <output_to_location_04> <figure_storage_path>

Options:
<output_location_from_02> directory containing wrangled_table.RDS from script 02.
<output_to_location_04> directory where numeric_feature_distributions.RDS will be saved.
<figure_storage_path> directory where numeric_feature_distributions.png will be saved.
" -> usage_doc

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
source(file.path(project_root, "src", "io_validation_utils.R"))
source(file.path(project_root, "src", "plot_numeric_distributions.R"))

run_numeric_features_distributions(
  output_location_from_02 = opt$output_location_from_02,
  output_to_location_04 = opt$output_to_location_04,
  figure_storage_path = opt$figure_storage_path
)
