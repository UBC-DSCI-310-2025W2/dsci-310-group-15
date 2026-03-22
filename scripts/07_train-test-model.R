"
Creates, fits, cross-validates, and tests a general linear model.
After that, it tabulates all the results and returns classification metrics and feature importances.

File path should always be relative and end with a backslash.

Usage: scripts/07_train-test-model.R <output_location_from_02> <figure_storage_path>

Options:
<output_location_from_02> location of the output for the tidied data (script 2) was stored.
<figure_storage_path> location where the .png of the plot will be stored.
" -> doc

required_packages <- c(
  "tidyverse", "lubridate", "caret",
  "scales", "patchwork", "purrr", "janitor", "pROC", "knitr", "docopt"
)

invisible(lapply(required_packages, library, character.only = TRUE))

opt <- docopt(doc)

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 5.5,
  fig.align = "center"
)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
)

# ---- Train/test split and logistic model ----
train_test_model <- function(output_location_from_02, figure_storage_path, processed_table_storage) {
  df_model <- readRDS(paste(output_location_from_02, 'wrangled_table.RDS', sep = ''))

  set.seed(123)
  train_idx <- createDataPartition(df_model$is_free, p = 0.80, list = FALSE)
  train_df <- df_model[train_idx, ]
  test_df <- df_model[-train_idx, ]

  predictor_names <- c(
    "required_age", "release_year", "game_type",
    "windows_support", "mac_support", "linux_support",
    "platform_count", "has_dlc", "has_demo", "n_categories",
    grep("^cat_", names(df_model), value = TRUE)
  )

  train_model <- train_df |>
    select(is_free, all_of(predictor_names))

  test_model <- test_df |>
    select(is_free, all_of(predictor_names))

  baseline_class <- train_model |>
    count(is_free, sort = TRUE) |>
    slice(1) |>
    pull(is_free) |>
    as.character()

  baseline_pred <- factor(
    rep(baseline_class, nrow(test_model)),
    levels = levels(test_model$is_free)
  )

  baseline_cm <- confusionMatrix(baseline_pred, test_model$is_free, positive = "Free")
  baseline_accuracy <- unname(baseline_cm$overall["Accuracy"])

  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )

  model_formula <- as.formula(
    paste("is_free ~", paste(predictor_names, collapse = " + "))
  )

  fit_glm <- caret::train(
    model_formula,
    data = train_model,
    method = "glm",
    family = binomial(),
    metric = "ROC",
    trControl = ctrl
  )

  prob_free <- predict(fit_glm, newdata = test_model, type = "prob")$Free
  pred_class <- factor(
    ifelse(prob_free >= 0.5, "Free", "Paid"),
    levels = levels(test_model$is_free)
  )

  cm <- confusionMatrix(pred_class, test_model$is_free, positive = "Free")
  roc_obj <- pROC::roc(
    response = test_model$is_free,
    predictor = prob_free,
    levels = c("Paid", "Free"),
    direction = "<"
  )

  eval_tbl <- tibble(
    metric = c(
      "Baseline accuracy (majority class)",
      "Model accuracy",
      "Sensitivity (Free recall)",
      "Specificity (Paid recall)",
      "Balanced accuracy",
      "ROC AUC"
    ),
    value = c(
      baseline_accuracy,
      unname(cm$overall["Accuracy"]),
      unname(cm$byClass["Sensitivity"]),
      unname(cm$byClass["Specificity"]),
      unname(cm$byClass["Balanced Accuracy"]),
      as.numeric(pROC::auc(roc_obj))
    )
  )

  evaluation_metrics_table <- eval_tbl |>
    mutate(value = round(value, 4))

  # ---- Confusion matrix plot ----
  cm_df <- as.data.frame(cm$table)

  confusion_matrix <- ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = scales::comma(Freq)), size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#DBEAFE", high = "#1D4ED8") +
    labs(
      title = "Held-Out Test Confusion Matrix",
      x = "Predicted class",
      y = "Observed class",
      fill = "Count"
    )

  # ---- ROC curve ----
  roc_df <- tibble(
    tpr = rev(roc_obj$sensitivities),
    fpr = rev(1 - roc_obj$specificities)
  )

  roc_curve <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
    geom_line(color = "#1D4ED8", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    coord_equal() +
    labs(
      title = paste0("ROC Curve (AUC = ", round(as.numeric(pROC::auc(roc_obj)), 3), ")"),
      x = "False Positive Rate",
      y = "True Positive Rate"
    )

  # ---- Highest-magnitude logistic coefficients ----
  coef_tbl <- summary(fit_glm$finalModel)$coefficients |>
    as.data.frame() |>
    rownames_to_column("term") |>
    as_tibble() |>
    mutate(
      odds_ratio = exp(Estimate),
      abs_estimate = abs(Estimate)
    ) |>
    filter(term != "(Intercept)") |>
    arrange(desc(abs_estimate))

  feature_importances <- coef_tbl |>
    select(term, Estimate, odds_ratio, `Pr(>|z|)`) |>
    slice_head(n = 12)

  #----Report visuals----
  write.csv(feature_importances, paste(figure_storage_path, 'feature_importances_table.csv', sep = ''), row.names = FALSE)
  write.csv(evaluation_metrics_table, paste(figure_storage_path, 'evaluation_metrics_table.csv', sep = ''), row.names = FALSE)
  ggsave(roc_curve, file = paste(figure_storage_path, 'roc_curve.png', sep = ''))
  ggsave(confusion_matrix, file = paste(figure_storage_path, 'confusion_matrix.png', sep = ''))
}

train_test_model(opt$output_location_from_02, opt$figure_storage_path)