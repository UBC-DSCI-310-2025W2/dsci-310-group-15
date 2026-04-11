"
Plots the rates of certain categories showing up in the targets in the wrangled data.

Usage: scripts/06_categorical-features-plots.R <games_wrangled_data_save_location> <rds_save_location> <figures_storage_path>

Options:
<games_wrangled_data_save_location> location of the wrangled table from 02_data-preprocessing.R is stored.
<rds_save_location> location where categorical_feat_gap.RDS will be stored.
<figures_storage_path> location where categorical_feat_gap.png will be stored.
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

# ---- Category-level predictive signals ----
categorical_plotter <- function(games_wrangled_data_save_location, output_to_location_06, figures_storage_path) {
  df_model <- readRDS(paste(games_wrangled_data_save_location, 'wrangled_table.RDS', sep = ''))

  # Rebuild cat_prev from the cat_ indicator columns already in df_model
  cat_cols <- grep("^cat_", names(df_model), value = TRUE)

  cat_prev <- df_model |>
    select(game_id, is_free, all_of(cat_cols)) |>
    pivot_longer(cols = all_of(cat_cols), names_to = "category", values_to = "present") |>
    filter(present) |>
    count(is_free, category) |>
    left_join(df_model |> count(is_free, name = "class_total"), by = "is_free") |>
    mutate(rate = n / class_total)

  cat_gap <- compute_category_gap(df_model)

  categorical_feat_gap <- plot_category_gap(cat_gap, top_n = 12)

  saveRDS(categorical_feat_gap, file = paste(output_to_location_06, 'categorical_feat_gap.RDS', sep = ''))
  ggsave(categorical_feat_gap, file = paste(figures_storage_path, 'categorical_feat_gap.png', sep = ''))
}

categorical_plotter(opt$games_wrangled_data_save_location, opt$rds_save_location, opt$figures_storage_path)