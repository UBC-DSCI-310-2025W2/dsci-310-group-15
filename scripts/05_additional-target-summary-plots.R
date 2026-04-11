"
Looks at the target variable using different plots to explore other ways of representing the class imbalance. Uses the wrangled data.

Usage: scripts/05_additional-target-summary.R <games_wrangled_data_save_location> <output_location_05> <figures_storage_path>

Options:
<games_wrangled_data_save_location> location of the wrangled table from 02_data-preprocessing.R is stored.
<rds_save_location> location where target_by_release_binary.RDS will be stored.
<figures_storage_path> location where target_by_release_binary.png will be stored.
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

#Source
source("R/plot_functions.R")

# ---- Additional predictor summaries ----
additional_target_plots <- function(games_wrangled_data_save_location, rds_save_location, figures_storage_path) {
  df_model <- readRDS(paste(games_wrangled_data_save_location, 'wrangled_table.RDS', sep = ''))

  release_by_class <- df_model |>
    filter(release_year > 0) |>
    count(release_year, is_free)

  p_release <- plot_release_year_by_class(df_model) # TODO: fix function

  binary_rates <- df_model |>
    group_by(is_free) |>
    summarise(
      `Has DLC` = mean(has_dlc),
      `Has Demo` = mean(has_demo),
      .groups = "drop"
    ) |>
    pivot_longer(cols = -is_free, names_to = "feature", values_to = "rate")

  p_binary <- plot_binary_feature_rates() #TODO: check if this is the correct call

  target_by_release_binary <- p_release / p_binary

  saveRDS(target_by_release_binary, file = paste(rds_save_location, 'target_by_release_binary.RDS', sep = ''))
  ggsave(target_by_release_binary, file = paste(figures_storage_path, 'target_by_release_binary.png', sep = ''))

}

additional_target_plots(opt$games_wrangled_data_save_location, opt$rds_save_location, opt$figures_storage_path)