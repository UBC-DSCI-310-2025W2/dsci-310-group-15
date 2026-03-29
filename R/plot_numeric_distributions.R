#' Pivot numeric predictors to long format for faceted plotting
#'
#' Converts wide modeling data into a long table with one row per
#' `(observation, predictor)` pair, which is a standard structure for faceted
#' histogram plotting. The target column is standardized to `target_class`
#' by default so downstream plotting code is dataset-agnostic.
#'
#' @param data A data frame containing a class/target column and numeric predictors.
#' @param target_col Name of the class/target column.
#' @param predictors Character vector of numeric predictor columns to include.
#' If `NULL`, all numeric columns except `target_col` are used.
#' @param class_col_name Name for the class column in the returned long data.
#' @param drop_non_positive_for Optional character vector of predictors for which
#' non-positive values should be removed (e.g., invalid years coded as `<= 0`).
#'
#' @return A tibble with `class_col_name`, `predictor`, and `value` columns.
#'
#' @examples
#' toy <- data.frame(
#'   outcome = factor(c("yes", "no")),
#'   x1 = c(0, 18),
#'   x2 = c(2020, 2019)
#' )
#' pivot_numeric_predictors(
#'   data = toy,
#'   target_col = "outcome",
#'   predictors = c("x1", "x2"),
#'   drop_non_positive_for = character(0)
#' )
pivot_numeric_predictors <- function(
    data,
    target_col = "is_free",
    predictors = NULL,
    class_col_name = "target_class",
    drop_non_positive_for = "release_year") {
  assert_single_string(target_col, "target_col")
  assert_single_string(class_col_name, "class_col_name")
  validate_required_columns(data, target_col, "data")

  if (missing(predictors) || is.null(predictors)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    predictors <- setdiff(numeric_cols, target_col)
  }

  if (!is.character(predictors) || length(predictors) == 0L) {
    stop("`predictors` must be a non-empty character vector.", call. = FALSE)
  }

  validate_required_columns(data, predictors, "data")

  non_numeric <- predictors[!vapply(data[predictors], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      sprintf(
        "All predictors must be numeric. Non-numeric columns: %s",
        paste(non_numeric, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.character(drop_non_positive_for)) {
    stop("`drop_non_positive_for` must be a character vector.", call. = FALSE)
  }

  numeric_long <- data |>
    dplyr::select(!!rlang::sym(target_col), dplyr::all_of(predictors)) |>
    dplyr::rename(!!class_col_name := !!rlang::sym(target_col)) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(class_col_name),
      names_to = "predictor",
      values_to = "value"
    )

  drop_cols <- intersect(drop_non_positive_for, predictors)
  if (length(drop_cols) > 0L) {
    numeric_long <- numeric_long |>
      dplyr::filter(!(predictor %in% drop_cols & value <= 0))
  }

  if (nrow(numeric_long) == 0L) {
    stop("No rows left after preprocessing numeric predictors.", call. = FALSE)
  }

  numeric_long
}

#' Build a faceted histogram grid for numeric predictors
#'
#' Produces a histogram grid with:
#' rows = class labels, columns = predictor names, and free x-scales per panel.
#' This makes it easier to compare predictor distributions across classes while
#' allowing each predictor to keep an appropriate axis range.
#'
#' @param numeric_long Long-format data returned by `pivot_numeric_predictors()`.
#' @param class_col Column containing class labels.
#' @param predictor_col Column containing predictor names.
#' @param value_col Column containing numeric values.
#' @param bins Number of histogram bins per facet.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' toy_long <- data.frame(
#'   target_class = factor(c("Free", "Free", "Paid", "Paid")),
#'   predictor = c("required_age", "platform_count", "required_age", "platform_count"),
#'   value = c(0, 2, 18, 1)
#' )
#' build_numeric_distribution_plot(toy_long)
build_numeric_distribution_plot <- function(
    numeric_long,
    class_col = "target_class",
    predictor_col = "predictor",
    value_col = "value",
    bins = 30L) {
  assert_single_string(class_col, "class_col")
  assert_single_string(predictor_col, "predictor_col")
  assert_single_string(value_col, "value_col")
  validate_required_columns(numeric_long, c(class_col, predictor_col, value_col), "numeric_long")

  if (!is.numeric(numeric_long[[value_col]])) {
    stop(sprintf("`numeric_long$%s` must be numeric.", value_col), call. = FALSE)
  }

  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) || bins < 1) {
    stop("`bins` must be a single positive number.", call. = FALSE)
  }

  plot_data <- numeric_long |>
    dplyr::transmute(
      class_value = .data[[class_col]],
      predictor_value = .data[[predictor_col]],
      numeric_value = .data[[value_col]]
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = numeric_value)) +
    ggplot2::geom_histogram(
      bins = as.integer(bins),
      fill = "#2563EB",
      color = "white",
      linewidth = 0.25
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(class_value),
      cols = ggplot2::vars(predictor_value),
      scales = "free_x"
    ) +
    ggplot2::labs(
      title = "Predictor Distributions by Class",
      x = "Value",
      y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Save numeric-distribution outputs
#'
#' Saves the histogram grid in two formats:
#' `.RDS` for reproducible reuse in R and `.png` for report-ready figures.
#'
#' @param numeric_grid_distribution A `ggplot` object.
#' @param output_object_dir Directory where the serialized `.RDS` plot is saved.
#' @param output_figure_dir Directory where the `.png` figure is saved.
#' @param object_filename Filename for the serialized plot object.
#' @param figure_filename Filename for the PNG figure.
#'
#' @return A named list containing `rds_path` and `png_path`.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
#' save_numeric_distribution_outputs(
#'   numeric_grid_distribution = p,
#'   output_object_dir = "results/",
#'   output_figure_dir = "results/"
#' )
#' }
save_numeric_distribution_outputs <- function(
    numeric_grid_distribution,
    output_object_dir,
    output_figure_dir,
    object_filename = "numeric_feature_distributions.RDS",
    figure_filename = "numeric_feature_distributions.png") {
  if (!inherits(numeric_grid_distribution, "ggplot")) {
    stop(
      "`numeric_grid_distribution` must be a ggplot object (for example, not a data.frame).",
      call. = FALSE
    )
  }

  ensure_directory_exists(output_object_dir, "output_object_dir")
  ensure_directory_exists(output_figure_dir, "output_figure_dir")
  assert_single_string(object_filename, "object_filename")
  assert_single_string(figure_filename, "figure_filename")

  rds_path <- build_file_path(output_object_dir, object_filename)
  png_path <- build_file_path(output_figure_dir, figure_filename)

  saveRDS(numeric_grid_distribution, file = rds_path)
  ggplot2::ggsave(plot = numeric_grid_distribution, filename = png_path, width = 10, height = 5.5)

  list(rds_path = rds_path, png_path = png_path)
}

#' Run the full numeric-distribution workflow
#'
#' End-to-end workflow for numeric predictor diagnostics:
#' 1) load serialized modeling data;
#' 2) convert selected numeric predictors to long format;
#' 3) build faceted histograms by class and predictor;
#' 4) save `.RDS` and `.png` outputs.
#'
#' @param input_data_dir Directory containing the input RDS data table.
#' @param output_object_dir Directory where serialized plot output is saved.
#' @param output_figure_dir Directory where figure output is saved.
#' @param input_filename Input filename inside `input_data_dir`.
#' @param target_col Target/class column used for facet rows.
#' @param predictors Numeric predictors to plot. If `NULL`, all numeric columns
#' except `target_col` are used.
#' @param drop_non_positive_for Predictors where non-positive values should be removed.
#'
#' @return A named list with:
#' \itemize{
#'   \item `numeric_long`: pivoted data for plotting
#'   \item `numeric_grid_distribution`: ggplot object
#'   \item `saved_paths`: output file paths
#' }
#'
#' @examples
#' \dontrun{
#' run_numeric_features_distributions(
#'   input_data_dir = "data/",
#'   output_object_dir = "results/",
#'   output_figure_dir = "results/",
#'   target_col = "is_free",
#'   predictors = c("x1", "x2")
#' )
#' }
run_numeric_features_distributions <- function(
    input_data_dir,
    output_object_dir,
    output_figure_dir,
    input_filename = "wrangled_table.RDS",
    target_col = "is_free",
    predictors = NULL,
    drop_non_positive_for = "release_year") {
  modeling_data <- load_wrangled_table(input_data_dir = input_data_dir, input_filename = input_filename)
  numeric_long <- pivot_numeric_predictors(
    data = modeling_data,
    target_col = target_col,
    predictors = predictors,
    class_col_name = "target_class",
    drop_non_positive_for = drop_non_positive_for
  )
  numeric_grid_distribution <- build_numeric_distribution_plot(
    numeric_long = numeric_long,
    class_col = "target_class",
    predictor_col = "predictor",
    value_col = "value"
  )
  saved_paths <- save_numeric_distribution_outputs(
    numeric_grid_distribution = numeric_grid_distribution,
    output_object_dir = output_object_dir,
    output_figure_dir = output_figure_dir
  )

  list(
    numeric_long = numeric_long,
    numeric_grid_distribution = numeric_grid_distribution,
    saved_paths = saved_paths
  )
}
