"
Looks at the target variable using different plots to explore other ways of representing the class imbalance.

File path should always be relative and end with a backslash.

Usage: scripts/05_additional-target-summary.R <output_location_from_02> <output_location_05> <figure_storage_path>

Options:
<output_location_from_02> location of the output for the tidied data (script 2) was stored.
<output_to_location_05> location where the output for this script will be stored.
<figure_storage_path> location where the .png of the plot will be stored.
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

  p_release <- plot_release_year_by_class(df_model)

  binary_rates <- df_model |>
    group_by(is_free) |>
    summarise(
      `Has DLC` = mean(has_dlc),
      `Has Demo` = mean(has_demo),
      .groups = "drop"
    ) |>
    pivot_longer(cols = -is_free, names_to = "feature", values_to = "rate")

  p_binary <- plot_binary_feature_rates()

  target_by_release_binary <- p_release / p_binary

  saveRDS(target_by_release_binary, file = paste(output_location_05, 'target_by_release_binary.RDS', sep = ''))
  ggsave(target_by_release_binary, file = paste(figure_storage_path, 'target_by_release_binary.png', sep = ''))

}

additional_target_plots(opt$output_location_from_02, opt$output_location_05, opt$figure_storage_path)