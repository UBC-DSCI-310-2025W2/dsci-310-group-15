resolve_project_root <- function() {
  if (dir.exists("R")) {
    return(".")
  }

  if (dir.exists(file.path("..", "..", "R"))) {
    return(file.path("..", ".."))
  }

  stop("Could not locate project root containing `R/`.", call. = FALSE)
}

project_root <- resolve_project_root()

source(file.path(project_root, "R", "io_validation_utils.R"))
source(file.path(project_root, "R", "plot_class_imbalance.R"))
source(file.path(project_root, "R", "plot_numeric_distributions.R"))
source(file.path(project_root, "R", "plot_target_summary.R"))
source(file.path(project_root, "R", "plot_categorical_features.R"))
source(file.path(project_root, "R", "model_training.R"))
source(file.path(project_root, "R", "train_test_split_data.R"))
