"
Plots the rates of certain categories showing up in the targets.

File path should always be relative and end with a backslash.

Usage: 06_categorical-features-plots.R <output_location_from_02> <output_location_06> <figure_storage_path>
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
categorical_plotter <- function() {
  df_model <- readRDS(paste(output_location_from_02, 'wrangled_table.RDS', sep = ''))

  cat_prev <- flat_data |>
    select(game_id, is_free, category_list) |>
    unnest(category_list) |>
    rename(category = category_list) |>
    filter(category %in% top_categories) |>
    count(is_free, category) |>
    left_join(df_model |> count(is_free, name = "class_total"), by = "is_free") |>
    mutate(rate = n / class_total)

  cat_gap <- cat_prev |>
    select(is_free, category, rate) |>
    pivot_wider(names_from = is_free, values_from = rate, values_fill = 0) |>
    mutate(diff_free_minus_paid = Free - Paid) |>
    arrange(desc(abs(diff_free_minus_paid)))

  categorical_feat_gap <- ggplot(
    cat_gap |> slice_head(n = 12),
    aes(
      x = reorder(category, diff_free_minus_paid),
      y = diff_free_minus_paid,
      fill = diff_free_minus_paid > 0
    )
  ) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(
      values = c("TRUE" = "#0EA5E9", "FALSE" = "#2563EB"),
      labels = c("FALSE" = "Higher for Paid", "TRUE" = "Higher for Free"),
      name = "Direction"
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Largest Class Differences in Top Category Prevalence",
      x = "Category",
      y = "Free Rate - Paid Rate"
  )

  saveRDS(categorical_feat_gap, file = paste(output_to_location_06, 'numeric-feature-distributions.png', sep = ''))
  ggsave(categorical_feat_gap, file = paste(figure_storage_path, 'categorical_feat_gap.png', sep = ''))

}