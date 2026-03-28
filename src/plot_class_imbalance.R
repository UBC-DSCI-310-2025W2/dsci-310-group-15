#' Summarize class distribution for a target column
#'
#' @param df_model A data frame containing the target column.
#' @param target_col Name of the target column. Defaults to `"is_free"`.
#'
#' @return A tibble with class counts, percentages, and text labels.
#' @examples
#' toy <- data.frame(is_free = factor(c("Free", "Paid", "Free")))
#' summarize_class_distribution(toy)
#'
summarize_class_distribution <- function(df_model, target_col = "is_free") {
  assert_single_string(target_col, "target_col")
  validate_required_columns(df_model, target_col, "df_model")

  counts <- dplyr::count(df_model, !!rlang::sym(target_col), name = "n")
  if (nrow(counts) == 0L) {
    stop("`df_model` has zero rows; cannot build class distribution.", call. = FALSE)
  }

  counts |>
    dplyr::mutate(
      pct = n / sum(n),
      label = paste0(scales::comma(n), " (", scales::percent(pct, accuracy = 0.1), ")")
    ) |>
    dplyr::rename(is_free = !!rlang::sym(target_col))
}

#' Build a class imbalance bar chart
#'
#' @param class_counts Output from `summarize_class_distribution()`.
#'
#' @return A ggplot object.
#' @examples
#' toy_counts <- data.frame(
#'   is_free = factor(c("Free", "Paid"), levels = c("Free", "Paid")),
#'   n = c(2, 1),
#'   pct = c(2 / 3, 1 / 3),
#'   label = c("2 (66.7%)", "1 (33.3%)")
#' )
#' build_class_distribution_plot(toy_counts)
#'
build_class_distribution_plot <- function(class_counts) {
  validate_required_columns(class_counts, c("is_free", "n", "label"), "class_counts")

  if (!is.numeric(class_counts$n)) {
    stop("`class_counts$n` must be numeric.", call. = FALSE)
  }

  ggplot2::ggplot(class_counts, ggplot2::aes(x = is_free, y = n, fill = is_free)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.4, size = 4.2) +
    ggplot2::scale_fill_manual(values = c("Free" = "#0EA5E9", "Paid" = "#2563EB")) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = "Class Balance in the Modeling Data",
      x = "Target Class",
      y = "Number of Games",
      fill = "Class"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Save class imbalance outputs
#'
#' @param class_distribution_plot A ggplot object.
#' @param output_to_location_03 Directory for the `.RDS` output.
#' @param figure_storage_path Directory for the `.png` output.
#' @param rds_filename Output filename for the `.RDS` file.
#' @param png_filename Output filename for the `.png` file.
#'
#' @return A named list with saved output paths.
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
#' save_class_distribution_outputs(p, "results/", "results/")
#' }
#'
save_class_distribution_outputs <- function(
    class_distribution_plot,
    output_to_location_03,
    figure_storage_path,
    rds_filename = "class_distribution_plot.RDS",
    png_filename = "class_distribution_plot.png") {
  if (!inherits(class_distribution_plot, "ggplot")) {
    stop("`class_distribution_plot` must be a ggplot object.", call. = FALSE)
  }

  ensure_directory_exists(output_to_location_03, "output_to_location_03")
  ensure_directory_exists(figure_storage_path, "figure_storage_path")

  rds_path <- build_file_path(output_to_location_03, rds_filename)
  png_path <- build_file_path(figure_storage_path, png_filename)

  saveRDS(class_distribution_plot, file = rds_path)
  ggplot2::ggsave(plot = class_distribution_plot, filename = png_path, width = 10, height = 5.5)

  list(rds_path = rds_path, png_path = png_path)
}

#' Run the full class imbalance workflow
#'
#' @param output_location_from_02 Directory containing `wrangled_table.RDS`.
#' @param output_to_location_03 Directory for the `.RDS` output.
#' @param figure_storage_path Directory for the `.png` output.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `class_counts`: summary tibble
#'   \item `class_distribution_plot`: ggplot object
#'   \item `saved_paths`: output file paths
#' }
#' @examples
#' \dontrun{
#' run_class_imbalance_check("data/", "results/", "results/")
#' }
#'
run_class_imbalance_check <- function(output_location_from_02, output_to_location_03, figure_storage_path) {
  df_model <- load_wrangled_table(output_location_from_02)
  class_counts <- summarize_class_distribution(df_model, target_col = "is_free")
  class_distribution_plot <- build_class_distribution_plot(class_counts)
  saved_paths <- save_class_distribution_outputs(
    class_distribution_plot = class_distribution_plot,
    output_to_location_03 = output_to_location_03,
    figure_storage_path = figure_storage_path
  )

  list(
    class_counts = class_counts,
    class_distribution_plot = class_distribution_plot,
    saved_paths = saved_paths
  )
}
