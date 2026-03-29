#' Summarize class distribution for a target column
#'
#' Computes per-class counts and percentages for any categorical target variable.
#' The returned table is standardized to `target_class`, `n`, `pct`, and `label`
#' so it can be reused by plotting/reporting functions across different datasets.
#'
#' @param data A data frame containing the target column.
#' @param target_col Name of the target/class column in `data`.
#' @param class_col_name Name of the class column in the returned summary table.
#'
#' @return A tibble with one row per class and columns:
#' `class_col_name`, `n` (count), `pct` (proportion), and `label`
#' (formatted `"count (percent)"` text).
#'
#' @examples
#' toy <- data.frame(outcome = factor(c("yes", "no", "yes")))
#' summarize_class_distribution(toy, target_col = "outcome")
summarize_class_distribution <- function(
    data,
    target_col = "is_free",
    class_col_name = "target_class") {
  assert_single_string(target_col, "target_col")
  assert_single_string(class_col_name, "class_col_name")
  validate_required_columns(data, target_col, "data")

  class_counts <- dplyr::count(data, !!rlang::sym(target_col), name = "n")
  if (nrow(class_counts) == 0L) {
    stop("`data` has zero rows; cannot compute class distribution.", call. = FALSE)
  }

  class_counts |>
    dplyr::mutate(
      pct = n / sum(n),
      label = paste0(scales::comma(n), " (", scales::percent(pct, accuracy = 0.1), ")")
    ) |>
    dplyr::rename(!!class_col_name := !!rlang::sym(target_col))
}

#' Build a class-distribution bar chart
#'
#' Builds a bar chart from a class summary table. Works for binary and multi-class
#' targets. If `fill_palette` is not supplied, it uses a predefined blue palette
#' for `Free/Paid` and otherwise generates a palette automatically.
#'
#' @param class_counts Output from `summarize_class_distribution()` or any data
#' frame with class/count/label columns.
#' @param class_col Column containing class labels.
#' @param count_col Column containing class counts.
#' @param label_col Column containing text labels printed above bars.
#' @param fill_palette Optional named character vector of fill colors.
#'
#' @return A `ggplot` bar chart object.
#'
#' @examples
#' toy_counts <- data.frame(
#'   target_class = factor(c("Free", "Paid"), levels = c("Free", "Paid")),
#'   n = c(2, 1),
#'   pct = c(2 / 3, 1 / 3),
#'   label = c("2 (66.7%)", "1 (33.3%)")
#' )
#' build_class_distribution_plot(toy_counts)
build_class_distribution_plot <- function(
    class_counts,
    class_col = "target_class",
    count_col = "n",
    label_col = "label",
    fill_palette = NULL) {
  assert_single_string(class_col, "class_col")
  assert_single_string(count_col, "count_col")
  assert_single_string(label_col, "label_col")
  validate_required_columns(class_counts, c(class_col, count_col, label_col), "class_counts")

  if (!is.numeric(class_counts[[count_col]])) {
    stop(sprintf("`class_counts$%s` must be numeric.", count_col), call. = FALSE)
  }

  plot_data <- class_counts |>
    dplyr::transmute(
      class_value = .data[[class_col]],
      count_value = .data[[count_col]],
      label_value = .data[[label_col]]
    )

  class_values <- unique(as.character(stats::na.omit(plot_data$class_value)))
  if (length(class_values) == 0L) {
    stop("`class_counts` has no non-missing class values.", call. = FALSE)
  }

  if (is.null(fill_palette)) {
    if (setequal(class_values, c("Free", "Paid"))) {
      fill_palette <- c("Free" = "#0EA5E9", "Paid" = "#2563EB")
    } else {
      fill_palette <- stats::setNames(scales::hue_pal()(length(class_values)), class_values)
    }
  }

  x_label <- tools::toTitleCase(gsub("_", " ", class_col, fixed = TRUE))

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = class_value, y = count_value, fill = class_value)
  ) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_value),
      vjust = -0.4,
      size = 4.2
    ) +
    ggplot2::scale_fill_manual(values = fill_palette, drop = FALSE, na.value = "grey70") +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = "Class Distribution in Modeling Data",
      x = x_label,
      y = "Count",
      fill = "Class"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Save class-distribution outputs
#'
#' Saves the class-distribution plot in two formats:
#' `.RDS` for reproducible downstream reuse in R, and `.png` for reports/slides.
#'
#' @param class_distribution_plot A `ggplot` object.
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
#' save_class_distribution_outputs(
#'   class_distribution_plot = p,
#'   output_object_dir = "results/",
#'   output_figure_dir = "results/"
#' )
#' }
save_class_distribution_outputs <- function(
    class_distribution_plot,
    output_object_dir,
    output_figure_dir,
    object_filename = "class_distribution_plot.RDS",
    figure_filename = "class_distribution_plot.png") {
  if (!inherits(class_distribution_plot, "ggplot")) {
    stop(
      "`class_distribution_plot` must be a ggplot object (for example, not a data.frame).",
      call. = FALSE
    )
  }

  ensure_directory_exists(output_object_dir, "output_object_dir")
  ensure_directory_exists(output_figure_dir, "output_figure_dir")
  assert_single_string(object_filename, "object_filename")
  assert_single_string(figure_filename, "figure_filename")

  rds_path <- build_file_path(output_object_dir, object_filename)
  png_path <- build_file_path(output_figure_dir, figure_filename)

  saveRDS(class_distribution_plot, file = rds_path)
  ggplot2::ggsave(plot = class_distribution_plot, filename = png_path, width = 10, height = 5.5)

  list(rds_path = rds_path, png_path = png_path)
}

#' Run the full class-distribution workflow
#'
#' End-to-end workflow for class-distribution analysis:
#' 1) load serialized modeling data;
#' 2) compute class counts/percentages;
#' 3) build a class-distribution plot;
#' 4) save `.RDS` and `.png` outputs.
#'
#' @param input_data_dir Directory containing the input RDS data table.
#' @param output_object_dir Directory where serialized plot output is saved.
#' @param output_figure_dir Directory where figure output is saved.
#' @param input_filename Input filename inside `input_data_dir`.
#' @param target_col Target/class column to summarize.
#'
#' @return A named list with:
#' \itemize{
#'   \item `class_counts`: summary tibble
#'   \item `class_distribution_plot`: ggplot object
#'   \item `saved_paths`: output file paths
#' }
#'
#' @examples
#' \dontrun{
#' run_class_imbalance_check(
#'   input_data_dir = "data/",
#'   output_object_dir = "results/",
#'   output_figure_dir = "results/",
#'   target_col = "is_free"
#' )
#' }
run_class_imbalance_check <- function(
    input_data_dir,
    output_object_dir,
    output_figure_dir,
    input_filename = "wrangled_table.RDS",
    target_col = "is_free") {
  modeling_data <- load_wrangled_table(input_data_dir = input_data_dir, input_filename = input_filename)
  class_counts <- summarize_class_distribution(data = modeling_data, target_col = target_col)
  class_distribution_plot <- build_class_distribution_plot(class_counts = class_counts)
  saved_paths <- save_class_distribution_outputs(
    class_distribution_plot = class_distribution_plot,
    output_object_dir = output_object_dir,
    output_figure_dir = output_figure_dir
  )

  list(
    class_counts = class_counts,
    class_distribution_plot = class_distribution_plot,
    saved_paths = saved_paths
  )
}
