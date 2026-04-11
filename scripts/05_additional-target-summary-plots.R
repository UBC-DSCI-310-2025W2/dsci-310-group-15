"
Looks at the target variable using different plots to explore other ways of representing the class imbalance.

Usage: scripts/05_additional-target-summary-plots.R <output_location_from_02> <output_location_05> <figure_storage_path>

Options:
<output_location_from_02>  Directory containing wrangled_table.RDS from script 02.
                           Must be a relative path ending with a trailing slash.
                           Example: data/
<output_location_05>       Directory where target_by_release_binary.RDS will be saved.
                           Must be a relative path ending with a trailing slash.
                           Example: results/
<figure_storage_path>      Directory where target_by_release_binary.png will be saved.
                           Must be a relative path ending with a trailing slash.
                           Example: results/
" -> doc

library(docopt)
required_packages <- c(
  "tidyverse", "caret",
  "scales", "patchwork", "janitor", "knitr"
)

invisible(lapply(required_packages, library, character.only = TRUE))

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

opt <- docopt(doc)

# ---- Additional predictor summaries ----
additional_target_plots <- function(output_location_from_02, output_location_05, figure_storage_path) {
  df_model <- readRDS(paste(output_location_from_02, 'wrangled_table.RDS', sep = ''))

  release_by_class <- df_model |>
    filter(release_year > 0) |>
    count(release_year, is_free)

  p_release <- ggplot(release_by_class, aes(x = release_year, y = n, fill = is_free)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Free" = "#4DBBEE", "Paid" = "#E87040"),
                      name = "Game Type") +
    labs(title = "Number of Games Released per Year by Class",
         x = "Release Year", y = "Count") +
    theme(legend.position = "right")

  binary_rates <- df_model |>
    group_by(is_free) |>
    summarise(
      `Has DLC` = mean(has_dlc),
      `Has Demo` = mean(has_demo),
      .groups = "drop"
    ) |>
    pivot_longer(cols = -is_free, names_to = "feature", values_to = "rate")

  p_binary <- ggplot(binary_rates, aes(x = feature, y = rate, fill = is_free)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("Free" = "#4DBBEE", "Paid" = "#E87040"),
                      name = "Game Type") +
    labs(title = "Binary Feature Rates by Class",
         x = "Feature", y = "Proportion of Games") +
    theme(legend.position = "right")

  target_by_release_binary <- p_release / p_binary

  saveRDS(target_by_release_binary, file = paste(output_location_05, 'target_by_release_binary.RDS', sep = ''))
  ggsave(target_by_release_binary, file = paste(figure_storage_path, 'target_by_release_binary.png', sep = ''))

}

additional_target_plots(opt$output_location_from_02, opt$output_location_05, opt$figure_storage_path)