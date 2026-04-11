"
Creates a class-imbalance bar chart from the wrangled table.

Usage: scripts/03_class-imbalance-check.R <output_location_from_02> <output_to_location_03> <figure_storage_path>

Options:
<output_location_from_02> directory containing wrangled_table.RDS from script 02.
<output_to_location_03> directory where class_distribution_plot.RDS will be saved.
<figure_storage_path> directory where class_distribution_plot.png will be saved.
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
source(file.path(project_root, "R", "plot_class_imbalance.R"))

run_class_imbalance_check(
  input_data_dir = opt$output_location_from_02,
  output_object_dir = opt$output_to_location_03,
  output_figure_dir = opt$figure_storage_path
)
