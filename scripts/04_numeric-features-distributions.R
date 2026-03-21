"
Creates a grid of histograms to explore the distribution of the numeric features.

File path should always be relative and end with a backslash.

Usage: 04_numeric-features-distributions.R <output_location_from_02> <output_to_location_04> <figure_storage_path>
" -> doc

library(docopt)
required_packages <- c(
  "jsonlite", "tidyverse",
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

# ---- Numeric predictor distributions ----

numeric_dists <- function(output_location_from_02, output_to_location_04, figure_storage_path) {

  df_model <- readRDS(paste(output_location_from_02, 'wrangled_table.RDS', sep = ''))

  numeric_long <- df_model |>
    select(is_free, required_age, release_year, platform_count, n_categories) |>
    pivot_longer(
      cols = -is_free,
      names_to = "predictor",
      values_to = "value"
    ) |>
    filter(!(predictor == "release_year" & value <= 0))

  numeric_grid_distribution <- ggplot(numeric_long, aes(x = value)) +
    geom_histogram(bins = 30, fill = "#2563EB", color = "white", linewidth = 0.25) +
    facet_grid(is_free ~ predictor, scales = "free_x") +
    labs(
      title = "Predictor Distributions by Class",
      x = "Value",
      y = "Count"
    )

saveRDS(numeric_grid_distribution, file = paste(output_to_location_04, 'numeric_feature_distributions.RDS', sep = ''))
ggsave(numeric_grid_distribution, file = paste(figure_storage_path, 'numeric_feature_distributions.png', sep = ''))

}

numeric_dists(opt$output_location_from_02, opt$output_to_location_04, opt$figure_storage_path)
