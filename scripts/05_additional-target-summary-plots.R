"
Looks at the target variable using different plots to explore other ways of representing the class imbalance.

File path should always be relative and end with a backslash.

Usage: 05_additional-target-summary.R <output_location_from_02> <output_location_05> <figure_storage_path>
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

  p_release <- ggplot(release_by_class, aes(x = release_year, y = n, color = is_free)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Free" = "#0EA5E9", "Paid" = "#2563EB")) +
    labs(
      title = "Release-Year Counts by Class",
      x = "Release Year",
      y = "Number of Games",
      color = "Class"
    )

  binary_rates <- df_model |>
    group_by(is_free) |>
    summarise(
      `Has DLC` = mean(has_dlc),
      `Has Demo` = mean(has_demo),
      .groups = "drop"
    ) |>
    pivot_longer(cols = -is_free, names_to = "feature", values_to = "rate")

  p_binary <- ggplot(binary_rates, aes(x = feature, y = rate, fill = is_free)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_text(
      aes(label = scales::percent(rate, accuracy = 0.1)),
      position = position_dodge(width = 0.7),
      vjust = -0.35,
      size = 3.8
    ) +
    scale_fill_manual(values = c("Free" = "#0EA5E9", "Paid" = "#2563EB")) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
    labs(
      title = "Binary Feature Rates by Class",
      x = "Feature",
      y = "Rate",
      fill = "Class"
    )

  target_by_release_binary <- p_release / p_binary

  saveRDS(target_by_release_binary, file = paste(output_location_05, 'target_by_release_binary.RDS', sep = ''))
  ggsave(target_by_release_binary, file = paste(figure_storage_path, 'target_by_release_binary.png', sep = ''))

}

additional_target_plots(opt$output_location_from_02, opt$output_location_05, opt$figure_storage_path)