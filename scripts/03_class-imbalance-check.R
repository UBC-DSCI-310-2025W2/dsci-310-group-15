"
Creates a bar chart for class imbalances. File path should always be relative and end with a backslash.

Usage: 03_class-imbalance-check.R <output_location_from_02> <output_to_location_03> <figure_storage_path>
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

# ---- Class balance ----

distribution_check <- function(output_location_from_02, output_to_location_03, figure_storage_path){

  df_model <- readRDS(paste(output_location_from_02, 'wrangled_table.RDS', sep = ''))

  class_counts <- df_model |>
    count(is_free) |>
    mutate(
      pct = n / sum(n),
      label = paste0(scales::comma(n), " (", scales::percent(pct, accuracy = 0.1), ")")
    )

  class_distribution_plot <- ggplot(class_counts, aes(x = is_free, y = n, fill = is_free)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = label), vjust = -0.4, size = 4.2) +
    scale_fill_manual(values = c("Free" = "#0EA5E9", "Paid" = "#2563EB")) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Class Balance in the Modeling Data",
      x = "Target Class",
      y = "Number of Games",
      fill = "Class"
    )

  saveRDS(class_distribution_plot, paste(output_to_location_03, 'wrangled_table.RDS', sep = ''))
  ggsave(class_distribution_plot, file = paste(figure_storage_path, 'class_distribution_plot.png', sep = ''))
}

distribution_check(opt$output_location_from_02, opt$output_to_location_03, opt$figure_storage_path)