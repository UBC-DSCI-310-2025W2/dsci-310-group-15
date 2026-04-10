"
Plots the rates of certain categories showing up in the targets.

Usage: scripts/06_categorical-features-plots.R <output_location_from_02> <output_location_06> <figure_storage_path>

Options:
<output_location_from_02>  Directory containing wrangled_table.RDS from script 02.
                           Must be a relative path ending with a trailing slash.
                           Example: data/
<output_location_06>       Directory where categorical_feat_gap.RDS will be saved.
                           Must be a relative path ending with a trailing slash.
                           Example: results/
<figure_storage_path>      Directory where categorical_feat_gap.png will be saved.
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

# ---- Category-level predictive signals ----
categorical_plotter <- function(output_location_from_02, output_to_location_06, figure_storage_path) {
  df_model <- readRDS(paste(output_location_from_02, 'wrangled_table.RDS', sep = ''))

  # Rebuild cat_prev from the cat_ indicator columns already in df_model
  cat_cols <- grep("^cat_", names(df_model), value = TRUE)

  cat_prev <- df_model |>
    select(game_id, is_free, all_of(cat_cols)) |>
    pivot_longer(cols = all_of(cat_cols), names_to = "category", values_to = "present") |>
    filter(present) |>
    count(is_free, category) |>
    left_join(df_model |> count(is_free, name = "class_total"), by = "is_free") |>
    mutate(rate = n / class_total)

  cat_gap <- cat_prev |>
    select(is_free, category, rate) |>
    pivot_wider(names_from = is_free, values_from = rate, values_fill = 0) |>
    mutate(gap = Free - Paid) |>
    arrange(desc(abs(gap))) |>
    slice_head(n = 12) |>
    mutate(
      direction = if_else(gap > 0, "More Free", "More Paid"),
      category = str_replace(category, "^cat_", "") |>
        str_replace_all("_", " ") |>
        str_to_title(),
      category = reorder(category, gap)
    )

  categorical_feat_gap <- ggplot(cat_gap, aes(x = category, y = gap, fill = direction)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("More Free" = "#4DBBEE", "More Paid" = "#E87040"),
                      name = NULL) +
    labs(
      title = "Category Prevalence Gap: Free vs Paid Games",
      subtitle = "Positive = more common in free games; Negative = more common in paid games",
      x = NULL,
      y = "Rate difference (Free − Paid)"
    )

  saveRDS(categorical_feat_gap, file = paste(output_to_location_06, 'categorical_feat_gap.RDS', sep = ''))
  ggsave(categorical_feat_gap, file = paste(figure_storage_path, 'categorical_feat_gap.png', sep = ''))
}

categorical_plotter(opt$output_location_from_02, opt$output_location_06, opt$figure_storage_path)