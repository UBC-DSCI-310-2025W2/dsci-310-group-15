#' Pivot numeric predictors to long format for faceted plotting
#'
#' @param df_model A data frame containing `is_free` and numeric predictors.
#' @param predictors Numeric predictor columns to include.
#'
#' @return A tibble in long format with columns `is_free`, `predictor`, and `value`.
#' @examples
#' toy <- data.frame(
#'   is_free = factor(c("Free", "Paid")),
#'   required_age = c(0, 18),
#'   release_year = c(2020, 2019),
#'   platform_count = c(2, 1),
#'   n_categories = c(5, 3)
#' )
#' pivot_numeric_predictors(toy)
#'
pivot_numeric_predictors <- function(
    df_model,
    predictors = c("required_age", "release_year", "platform_count", "n_categories")) {
  validate_required_columns(df_model, c("is_free", predictors), "df_model")

  non_numeric <- predictors[!vapply(df_model[predictors], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      sprintf(
        "All predictors must be numeric. Non-numeric columns: %s",
        paste(non_numeric, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  numeric_long <- df_model |>
    dplyr::select(is_free, dplyr::all_of(predictors)) |>
    tidyr::pivot_longer(
      cols = -is_free,
      names_to = "predictor",
      values_to = "value"
    )

  if ("release_year" %in% predictors) {
    numeric_long <- numeric_long |>
      dplyr::filter(!(predictor == "release_year" & value <= 0))
  }

  if (nrow(numeric_long) == 0L) {
    stop("No rows left after preprocessing numeric predictors.", call. = FALSE)
  }

  numeric_long
}

#' Build a faceted histogram grid for numeric predictors
#'
#' @param numeric_long Output from `pivot_numeric_predictors()`.
#'
#' @return A ggplot object.
#' @examples
#' toy_long <- data.frame(
#'   is_free = factor(c("Free", "Free", "Paid", "Paid")),
#'   predictor = c("required_age", "platform_count", "required_age", "platform_count"),
#'   value = c(0, 2, 18, 1)
#' )
#' build_numeric_distribution_plot(toy_long)
#'
build_numeric_distribution_plot <- function(numeric_long) {
  validate_required_columns(numeric_long, c("is_free", "predictor", "value"), "numeric_long")

  if (!is.numeric(numeric_long$value)) {
    stop("`numeric_long$value` must be numeric.", call. = FALSE)
  }

  ggplot2::ggplot(numeric_long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 30, fill = "#2563EB", color = "white", linewidth = 0.25) +
    ggplot2::facet_grid(is_free ~ predictor, scales = "free_x") +
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

#' Save numeric distribution outputs
#'
#' @param numeric_grid_distribution A ggplot object.
#' @param output_to_location_04 Directory for the `.RDS` output.
#' @param figure_storage_path Directory for the `.png` output.
#' @param rds_filename Output filename for the `.RDS` file.
#' @param png_filename Output filename for the `.png` file.
#'
#' @return A named list with saved output paths.
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
#' save_numeric_distribution_outputs(p, "results/", "results/")
#' }
#'
save_numeric_distribution_outputs <- function(
    numeric_grid_distribution,
    output_to_location_04,
    figure_storage_path,
    rds_filename = "numeric_feature_distributions.RDS",
    png_filename = "numeric_feature_distributions.png") {
  if (!inherits(numeric_grid_distribution, "ggplot")) {
    stop("`numeric_grid_distribution` must be a ggplot object.", call. = FALSE)
  }

  ensure_directory_exists(output_to_location_04, "output_to_location_04")
  ensure_directory_exists(figure_storage_path, "figure_storage_path")

  rds_path <- build_file_path(output_to_location_04, rds_filename)
  png_path <- build_file_path(figure_storage_path, png_filename)

  saveRDS(numeric_grid_distribution, file = rds_path)
  ggplot2::ggsave(plot = numeric_grid_distribution, filename = png_path, width = 10, height = 5.5)

  list(rds_path = rds_path, png_path = png_path)
}

#' Run the full numeric feature distribution workflow
#'
#' @param output_location_from_02 Directory containing `wrangled_table.RDS`.
#' @param output_to_location_04 Directory for the `.RDS` output.
#' @param figure_storage_path Directory for the `.png` output.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `numeric_long`: pivoted data for plotting
#'   \item `numeric_grid_distribution`: ggplot object
#'   \item `saved_paths`: output file paths
#' }
#' @examples
#' \dontrun{
#' run_numeric_features_distributions("data/", "results/", "results/")
#' }
#'
run_numeric_features_distributions <- function(output_location_from_02, output_to_location_04, figure_storage_path) {
  df_model <- load_wrangled_table(output_location_from_02)
  numeric_long <- pivot_numeric_predictors(df_model)
  numeric_grid_distribution <- build_numeric_distribution_plot(numeric_long)
  saved_paths <- save_numeric_distribution_outputs(
    numeric_grid_distribution = numeric_grid_distribution,
    output_to_location_04 = output_to_location_04,
    figure_storage_path = figure_storage_path
  )

  list(
    numeric_long = numeric_long,
    numeric_grid_distribution = numeric_grid_distribution,
    saved_paths = saved_paths
  )
}
