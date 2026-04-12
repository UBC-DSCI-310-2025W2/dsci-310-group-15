#' Compute category prevalence gaps between two target classes
#'
#' @param data Modeling data frame containing category indicator columns.
#' @param target_col Target/class column.
#' @param id_col Observation identifier column.
#' @param category_prefix Prefix used by category indicator columns.
#' @param level_a First target level. Positive gaps mean higher prevalence here.
#' @param level_b Second target level.
#' @param top_n Number of largest absolute gaps to return.
#'
#' @return A tibble with category rates and gaps.
compute_category_prevalence_gap <- function(
    data,
    target_col = "is_free",
    id_col = "game_id",
    category_prefix = "cat_",
    level_a = "Free",
    level_b = "Paid",
    top_n = 12L) {
  validate_required_columns(data, c(id_col, target_col), "data")
  assert_single_string(category_prefix, "category_prefix")
  assert_single_string(level_a, "level_a")
  assert_single_string(level_b, "level_b")

  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive number.", call. = FALSE)
  }

  category_columns <- grep(paste0("^", category_prefix), names(data), value = TRUE)
  if (length(category_columns) == 0L) {
    stop(sprintf("No category columns found with prefix `%s`.", category_prefix), call. = FALSE)
  }

  category_prevalence <- data |>
    dplyr::select(dplyr::all_of(c(id_col, target_col, category_columns))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(category_columns),
      names_to = "category",
      values_to = "present"
    ) |>
    dplyr::filter(as.logical(present)) |>
    dplyr::count(.data[[target_col]], category, name = "n") |>
    dplyr::left_join(
      data |> dplyr::count(.data[[target_col]], name = "class_total"),
      by = target_col
    ) |>
    dplyr::mutate(rate = n / class_total)

  gap_table <- category_prevalence |>
    dplyr::select(dplyr::all_of(target_col), category, rate) |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(target_col),
      values_from = rate,
      values_fill = 0
    )

  if (!level_a %in% names(gap_table)) {
    gap_table[[level_a]] <- 0
  }
  if (!level_b %in% names(gap_table)) {
    gap_table[[level_b]] <- 0
  }

  gap_table |>
    dplyr::mutate(
      gap = .data[[level_a]] - .data[[level_b]],
      direction = dplyr::if_else(gap > 0, paste("More", level_a), paste("More", level_b)),
      category_label = category |>
        stringr::str_replace(paste0("^", category_prefix), "") |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_title()
    ) |>
    dplyr::arrange(dplyr::desc(abs(gap))) |>
    dplyr::slice_head(n = as.integer(top_n))
}

#' Build a category prevalence gap plot
#'
#' @param category_gap Output from [compute_category_prevalence_gap()].
#' @param level_a First target level.
#' @param level_b Second target level.
#'
#' @return A ggplot object.
build_category_gap_plot <- function(
    category_gap,
    level_a = "Free",
    level_b = "Paid") {
  validate_required_columns(category_gap, c("category_label", "gap", "direction"), "category_gap")
  positive_label <- paste("More", level_a)
  negative_label <- paste("More", level_b)
  fill_values <- stats::setNames(
    c("#4DBBEE", "#E87040"),
    c(positive_label, negative_label)
  )

  ggplot2::ggplot(
    category_gap,
    ggplot2::aes(x = stats::reorder(category_label, gap), y = gap, fill = direction)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = fill_values, name = NULL) +
    ggplot2::labs(
      title = "Category Prevalence Gap: Free vs Paid Games",
      subtitle = "Positive = more common in free games; negative = more common in paid games",
      x = NULL,
      y = "Rate difference (Free - Paid)"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Save category gap plot outputs
#'
#' @param category_gap_plot Plot to save.
#' @param output_object_dir Directory for the RDS plot object.
#' @param output_figure_dir Directory for the PNG figure.
#' @param object_filename Output RDS filename.
#' @param figure_filename Output PNG filename.
#'
#' @return A named list with output paths.
save_category_gap_outputs <- function(
    category_gap_plot,
    output_object_dir,
    output_figure_dir,
    object_filename = "categorical_feat_gap.RDS",
    figure_filename = "categorical_feat_gap.png") {
  ensure_directory_exists(output_object_dir, "output_object_dir")
  ensure_directory_exists(output_figure_dir, "output_figure_dir")

  rds_path <- build_file_path(output_object_dir, object_filename)
  png_path <- build_file_path(output_figure_dir, figure_filename)

  saveRDS(category_gap_plot, rds_path)
  ggplot2::ggsave(plot = category_gap_plot, filename = png_path, width = 10, height = 5.5)

  list(rds_path = rds_path, png_path = png_path)
}

#' Run the categorical feature plotting workflow
#'
#' @param input_data_dir Directory containing `wrangled_table.RDS`.
#' @param output_object_dir Directory where the RDS plot object is saved.
#' @param output_figure_dir Directory where the PNG figure is saved.
#' @param input_filename Input RDS filename under `input_data_dir`.
#'
#' @return A named list with gap table, plot, and output paths.
run_categorical_features_plot <- function(
    input_data_dir,
    output_object_dir,
    output_figure_dir,
    input_filename = "wrangled_table.RDS") {
  modeling_data <- processandplot::load_wrangled_table(input_data_dir, input_filename)
  category_gap <- compute_category_prevalence_gap(modeling_data)
  category_gap_plot <- build_category_gap_plot(category_gap)
  saved_paths <- save_category_gap_outputs(
    category_gap_plot,
    output_object_dir = output_object_dir,
    output_figure_dir = output_figure_dir
  )

  list(
    category_gap = category_gap,
    category_gap_plot = category_gap_plot,
    saved_paths = saved_paths
  )
}
