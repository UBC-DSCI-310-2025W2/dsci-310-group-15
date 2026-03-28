resolve_project_root <- function() {
  if (dir.exists("src")) {
    return(".")
  }

  if (dir.exists(file.path("..", "..", "src"))) {
    return(file.path("..", ".."))
  }

  stop("Could not locate project root containing `src/`.", call. = FALSE)
}

project_root <- resolve_project_root()

source(file.path(project_root, "src", "io_validation_utils.R"))
source(file.path(project_root, "src", "plot_class_imbalance.R"))
source(file.path(project_root, "src", "plot_numeric_distributions.R"))
