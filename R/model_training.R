#' Select predictor columns for the Steam classification model
#'
#' @param data Modeling data frame.
#' @param category_pattern Regular expression matching one-hot category columns.
#'
#' @return Character vector of predictor column names.
select_model_predictors <- function(data, category_pattern = "^cat_") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  base_predictors <- c(
    "required_age",
    "release_year",
    "game_type",
    "windows_support",
    "mac_support",
    "linux_support",
    "platform_count",
    "has_dlc",
    "has_demo",
    "n_categories"
  )
  category_predictors <- grep(category_pattern, names(data), value = TRUE)
  predictors <- c(base_predictors, category_predictors)

  validate_required_columns(data, predictors, "data")
  predictors
}

#' Create complementary stratified train and test splits
#'
#' @param data Data frame to split.
#' @param target_col Target/class column used for stratification.
#' @param train_prop Proportion of rows assigned to training.
#' @param seed Optional random seed for reproducible splitting.
#'
#' @return A list containing train/test data and train/test row indices.
create_stratified_train_test_split <- function(
    data,
    target_col = "is_free",
    train_prop = 0.8,
    seed = 123) {
  validate_required_columns(data, target_col, "data")

  if (!is.numeric(train_prop) || length(train_prop) != 1L ||
      is.na(train_prop) || train_prop <= 0 || train_prop >= 1) {
    stop("`train_prop` must be a single number between 0 and 1.", call. = FALSE)
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("`seed` must be `NULL` or a single number.", call. = FALSE)
    }
    set.seed(seed)
  }

  train_indices <- as.integer(caret::createDataPartition(
    data[[target_col]],
    p = train_prop,
    list = FALSE
  ))
  all_indices <- seq_len(nrow(data))
  test_indices <- setdiff(all_indices, train_indices)

  list(
    train_data = data[train_indices, , drop = FALSE],
    test_data = data[test_indices, , drop = FALSE],
    train_indices = train_indices,
    test_indices = test_indices
  )
}

#' Build model evaluation metrics
#'
#' @param fit_glm Trained caret model.
#' @param test_model Test data with target and predictors.
#' @param target_col Target/class column.
#' @param positive_class Positive class label.
#'
#' @return A named list with predictions, confusion matrices, ROC, and metrics.
evaluate_logistic_model <- function(
    fit_glm,
    test_model,
    target_col = "is_free",
    positive_class = "Free") {
  validate_required_columns(test_model, target_col, "test_model")

  prob_positive <- predict(fit_glm, newdata = test_model, type = "prob")[[positive_class]]
  pred_class <- factor(
    ifelse(prob_positive >= 0.5, positive_class, "Paid"),
    levels = levels(test_model[[target_col]])
  )

  confusion_matrix <- caret::confusionMatrix(
    pred_class,
    test_model[[target_col]],
    positive = positive_class
  )
  roc_obj <- pROC::roc(
    response = test_model[[target_col]],
    predictor = prob_positive,
    levels = c("Paid", positive_class),
    direction = "<",
    quiet = TRUE
  )

  list(
    prob_positive = prob_positive,
    pred_class = pred_class,
    confusion_matrix = confusion_matrix,
    roc_obj = roc_obj
  )
}

#' Build an evaluation metrics table
#'
#' @param baseline_accuracy Accuracy from the majority-class baseline.
#' @param confusion_matrix Confusion matrix for the trained model.
#' @param roc_obj ROC object for the trained model.
#'
#' @return A tibble of rounded metric values.
build_evaluation_metrics_table <- function(baseline_accuracy, confusion_matrix, roc_obj) {
  tibble::tibble(
    metric = c(
      "Baseline accuracy (majority class)",
      "Model accuracy",
      "Cohen's Kappa",
      "Sensitivity (Free recall)",
      "Specificity (Paid recall)",
      "Balanced accuracy",
      "ROC AUC"
    ),
    value = c(
      baseline_accuracy,
      unname(confusion_matrix$overall["Accuracy"]),
      unname(confusion_matrix$overall["Kappa"]),
      unname(confusion_matrix$byClass["Sensitivity"]),
      unname(confusion_matrix$byClass["Specificity"]),
      unname(confusion_matrix$byClass["Balanced Accuracy"]),
      as.numeric(pROC::auc(roc_obj))
    )
  ) |>
    dplyr::mutate(value = round(value, 4))
}

#' Build a confusion matrix heatmap
#'
#' @param confusion_matrix caret confusion matrix object.
#'
#' @return A ggplot object.
build_confusion_matrix_plot <- function(confusion_matrix) {
  cm_df <- as.data.frame(confusion_matrix$table)

  ggplot2::ggplot(cm_df, ggplot2::aes(x = Prediction, y = Reference, fill = Freq)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(Freq)), size = 5, fontface = "bold") +
    ggplot2::scale_fill_gradient(low = "#DBEAFE", high = "#1D4ED8") +
    ggplot2::labs(
      title = "Held-Out Test Confusion Matrix",
      x = "Predicted class",
      y = "Observed class",
      fill = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

#' Build an ROC curve plot
#'
#' @param roc_obj ROC object from pROC.
#'
#' @return A ggplot object.
build_roc_curve_plot <- function(roc_obj) {
  roc_df <- tibble::tibble(
    tpr = rev(roc_obj$sensitivities),
    fpr = rev(1 - roc_obj$specificities)
  )

  ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_line(color = "#1D4ED8", linewidth = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste0("ROC Curve (AUC = ", round(as.numeric(pROC::auc(roc_obj)), 3), ")"),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

#' Extract largest-magnitude logistic model coefficients
#'
#' @param fit_glm Trained caret GLM model.
#' @param top_n Number of rows to keep.
#'
#' @return A tibble with coefficient terms, estimates, odds ratios, and p-values.
extract_feature_importances <- function(fit_glm, top_n = 15L) {
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive number.", call. = FALSE)
  }

  summary(fit_glm$finalModel)$coefficients |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      odds_ratio = exp(Estimate),
      abs_estimate = abs(Estimate)
    ) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::arrange(dplyr::desc(abs_estimate)) |>
    dplyr::select(term, Estimate, odds_ratio, `Pr(>|z|)`) |>
    dplyr::slice_head(n = as.integer(top_n))
}

#' Run model training, evaluation, and output writing
#'
#' @param input_data_dir Directory containing `wrangled_table.RDS`.
#' @param results_dir Directory where figures and tables are saved.
#' @param input_filename Input RDS filename under `input_data_dir`.
#' @param train_prop Proportion of rows used for training.
#' @param seed Random seed for the split and caret training.
#'
#' @return A named list containing trained model objects, plots, tables, and paths.
run_train_test_model <- function(
    input_data_dir,
    results_dir,
    input_filename = "wrangled_table.RDS",
    train_prop = 0.8,
    seed = 123) {
  modeling_data <- load_wrangled_table(input_data_dir, input_filename)
  ensure_directory_exists(results_dir, "results_dir")
  predictors <- select_model_predictors(modeling_data)
  split <- create_stratified_train_test_split(
    modeling_data,
    target_col = "is_free",
    train_prop = train_prop,
    seed = seed
  )
  model_input_validation <- validate_modeling_table(
    modeling_data,
    stage = "model_input"
  )
  training_validation <- validate_training_correlations(
    split$train_data,
    predictors = predictors,
    target_col = "is_free",
    stage = "training_split"
  )
  validation_report <- combine_validation_reports(model_input_validation, training_validation)
  validation_report_path <- save_validation_report(
    validation_report,
    results_dir,
    "train_validation_report.csv"
  )
  assert_no_validation_failures(validation_report)

  train_model <- split$train_data |>
    dplyr::select(is_free, dplyr::all_of(predictors))
  test_model <- split$test_data |>
    dplyr::select(is_free, dplyr::all_of(predictors))

  baseline_class <- train_model |>
    dplyr::count(is_free, sort = TRUE) |>
    dplyr::slice(1) |>
    dplyr::pull(is_free) |>
    as.character()

  baseline_pred <- factor(
    rep(baseline_class, nrow(test_model)),
    levels = levels(test_model$is_free)
  )
  baseline_cm <- caret::confusionMatrix(baseline_pred, test_model$is_free, positive = "Free")
  baseline_accuracy <- unname(baseline_cm$overall["Accuracy"])

  train_control <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary
  )
  model_formula <- stats::as.formula(
    paste("is_free ~", paste(predictors, collapse = " + "))
  )

  set.seed(seed)
  fit_glm <- caret::train(
    model_formula,
    data = train_model,
    method = "glm",
    family = stats::binomial(),
    metric = "ROC",
    trControl = train_control
  )

  evaluation <- evaluate_logistic_model(fit_glm, test_model)
  evaluation_metrics_table <- build_evaluation_metrics_table(
    baseline_accuracy,
    evaluation$confusion_matrix,
    evaluation$roc_obj
  )
  confusion_matrix_plot <- build_confusion_matrix_plot(evaluation$confusion_matrix)
  roc_curve_plot <- build_roc_curve_plot(evaluation$roc_obj)
  feature_importances <- extract_feature_importances(fit_glm, top_n = 15L)

  output_paths <- list(
    feature_importances = build_file_path(results_dir, "feature_importances_table.csv"),
    evaluation_metrics = build_file_path(results_dir, "evaluation_metrics_table.csv"),
    roc_curve = build_file_path(results_dir, "roc_curve.png"),
    confusion_matrix = build_file_path(results_dir, "confusion_matrix.png"),
    train_validation_report = validation_report_path
  )

  utils::write.csv(feature_importances, output_paths$feature_importances, row.names = FALSE)
  utils::write.csv(evaluation_metrics_table, output_paths$evaluation_metrics, row.names = FALSE)
  ggplot2::ggsave(plot = roc_curve_plot, filename = output_paths$roc_curve, width = 7, height = 6)
  ggplot2::ggsave(plot = confusion_matrix_plot, filename = output_paths$confusion_matrix, width = 7, height = 6)

  list(
    fit_glm = fit_glm,
    split = split,
    evaluation = evaluation,
    evaluation_metrics_table = evaluation_metrics_table,
    feature_importances = feature_importances,
    confusion_matrix_plot = confusion_matrix_plot,
    roc_curve_plot = roc_curve_plot,
    validation_report = validation_report,
    output_paths = output_paths
  )
}
