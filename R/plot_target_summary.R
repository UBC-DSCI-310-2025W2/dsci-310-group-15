#' Build release-year counts by class
#'
#' @param data Modeling data frame.
#' @param target_col Target/class column.
#' @param release_year_col Release-year column.
#'
#' @return A tibble with release-year/class counts.
summarize_release_counts_by_class <- function(
    data,
    target_col = "is_free",
    release_year_col = "release_year") {
  processandplotr::validate_required_columns(data, c(target_col, release_year_col), "data")

  data |>
    dplyr::filter(.data[[release_year_col]] > 0) |>
    dplyr::count(.data[[release_year_col]], .data[[target_col]], name = "n") |>
    dplyr::rename(release_year = dplyr::all_of(release_year_col))
}

#' Build binary feature rates by class
#'
#' @param data Modeling data frame.
#' @param target_col Target/class column.
#' @param binary_cols Logical feature columns to summarize.
#' @param feature_labels Labels to use in the plot.
#'
#' @return A long-format tibble with one rate per target class and feature.
summarize_binary_feature_rates <- function(
    data,
    target_col = "is_free",
    binary_cols = c("has_dlc", "has_demo"),
    feature_labels = c(has_dlc = "Has DLC", has_demo = "Has Demo")) {
  processandplotr::validate_required_columns(data, c(target_col, binary_cols), "data")

  rates <- data |>
    dplyr::group_by(.data[[target_col]]) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(binary_cols), ~ mean(as.logical(.x), na.rm = TRUE)),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(target_col),
      names_to = "feature",
      values_to = "rate"
    )

  rates$feature <- dplyr::recode(rates$feature, !!!feature_labels)
  rates
}

#' Build the release-year class-count plot
#'
#' @param release_counts Output from [summarize_release_counts_by_class()].
#' @param target_col Target/class column.
#'
#' @return A ggplot object.
build_release_counts_plot <- function(release_counts, target_col = "is_free") {
  processandplotr::validate_required_columns(release_counts, c("release_year", target_col, "n"), "release_counts")

  ggplot2::ggplot(
    release_counts,
    ggplot2::aes(x = release_year, y = n, fill = .data[[target_col]])
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(values = c("Free" = "#4DBBEE", "Paid" = "#E87040"), name = "Game Type") +
    ggplot2::labs(
      title = "Number of Games Released per Year by Class",
      x = "Release Year",
      y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Build the binary feature rate plot
#'
#' @param binary_rates Output from [summarize_binary_feature_rates()].
#' @param target_col Target/class column.
#'
#' @return A ggplot object.
build_binary_feature_rates_plot <- function(binary_rates, target_col = "is_free") {
  processandplotr::validate_required_columns(binary_rates, c(target_col, "feature", "rate"), "binary_rates")

  ggplot2::ggplot(
    binary_rates,
    ggplot2::aes(x = feature, y = rate, fill = .data[[target_col]])
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = c("Free" = "#4DBBEE", "Paid" = "#E87040"), name = "Game Type") +
    ggplot2::labs(
      title = "Binary Feature Rates by Class",
      x = "Feature",
      y = "Proportion of Games"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Build the combined target-summary plot
#'
#' @param data Modeling data frame.
#' @param target_col Target/class column.
#'
#' @return A patchwork/ggplot object.
build_target_summary_plot <- function(data, target_col = "is_free") {
  release_counts <- summarize_release_counts_by_class(data, target_col = target_col)
  binary_rates <- summarize_binary_feature_rates(data, target_col = target_col)

  patchwork::wrap_plots(
    build_release_counts_plot(release_counts, target_col = target_col),
    build_binary_feature_rates_plot(binary_rates, target_col = target_col),
    ncol = 1
  )
}

#' Save target-summary outputs
#'
#' @param target_summary_plot Plot to save.
#' @param output_object_dir Directory for the RDS plot object.
#' @param output_figure_dir Directory for the PNG figure.
#' @param object_filename Output RDS filename.
#' @param figure_filename Output PNG filename.
#'
#' @return A named list with output paths.
save_target_summary_outputs <- function(
    target_summary_plot,
    output_object_dir,
    output_figure_dir,
    object_filename = "target_by_release_binary.RDS",
    figure_filename = "target_by_release_binary.png") {
  processandplotr::ensure_directory_exists(output_object_dir, "output_object_dir")
  processandplotr::ensure_directory_exists(output_figure_dir, "output_figure_dir")

  rds_path <- processandplotr::build_file_path(output_object_dir, object_filename)
  png_path <- processandplotr::build_file_path(output_figure_dir, figure_filename)

  saveRDS(target_summary_plot, rds_path)
  ggplot2::ggsave(plot = target_summary_plot, filename = png_path, width = 10, height = 5.5)

  list(rds_path = rds_path, png_path = png_path)
}

#' Run the target-summary plotting workflow
#'
#' @param input_data_dir Directory containing `wrangled_table.RDS`.
#' @param output_object_dir Directory where the RDS plot object is saved.
#' @param output_figure_dir Directory where the PNG figure is saved.
#' @param input_filename Input RDS filename under `input_data_dir`.
#'
#' @return A named list with plot and output paths.
run_target_summary_plots <- function(
    input_data_dir,
    output_object_dir,
    output_figure_dir,
    input_filename = "wrangled_table.RDS") {
  modeling_data <- processandplotr::load_wrangled_table(input_data_dir, input_filename)
  target_summary_plot <- build_target_summary_plot(modeling_data)
  saved_paths <- save_target_summary_outputs(
    target_summary_plot,
    output_object_dir = output_object_dir,
    output_figure_dir = output_figure_dir
  )

  list(
    target_summary_plot = target_summary_plot,
    saved_paths = saved_paths
  )
}
