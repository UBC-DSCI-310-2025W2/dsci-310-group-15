#' Steam Game Analysis Plot Functions
#'
#' A collection of reusable ggplot2 helper functions for visualising
#' free-vs-paid class differences in Steam game data. Each function
#' accepts a pre-processed data frame and returns a ggplot object,
#' keeping side-effects (saving files) in the calling scripts.
#'
#' Dependencies: tidyverse, scales, patchwork

library(tidyverse)
library(scales)
library(patchwork)


# 1. plot_release_year_by_class

#' Plot release-year game counts split by free/paid class
#'
#' Creates a line chart showing the number of games released each year,
#' with separate lines for free and paid titles. Years with a
#' non-positive release_year value (e.g. -1 used as a missing-value
#' sentinel) are excluded before counting.
#'
#' @param df A data frame containing at least the columns `release_year`
#'   (numeric) and `is_free` (factor with levels `"Free"` and `"Paid"`).
#' @param free_colour A length-1 character string giving the hex colour
#'   used for the "Free" line. Defaults to `"#0EA5E9"`.
#' @param paid_colour A length-1 character string giving the hex colour
#'   used for the "Paid" line. Defaults to `"#2563EB"`.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- data.frame(
#'   release_year = c(2018, 2019, 2019, 2020),
#'   is_free = factor(c("Free", "Paid", "Paid", "Free"),
#'                    levels = c("Free", "Paid"))
#' )
#' plot_release_year_by_class(df)
plot_release_year_by_class <- function(df,
                                       free_colour = "#0EA5E9",
                                       paid_colour = "#2563EB") {

  stopifnot(
    is.data.frame(df),
    "release_year" %in% names(df),
    "is_free" %in% names(df),
    is.character(free_colour), length(free_colour) == 1,
    is.character(paid_colour), length(paid_colour) == 1
  )

  df_plot <- df[df$release_year > 0, ]

  ggplot(mapping = aes(x = release_year), data = df_plot) +
    geom_histogram(
      aes(colour = is_free),
      binwidth = 1,
      fill = NA
    ) +
    scale_colour_manual(
      values = c("Free" = free_colour, "Paid" = paid_colour)
    ) +
    labs(
      title = "Release-Year Counts by Class",
      x     = "Release Year",
      y     = "Number of Games",
      colour = "Class"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}


# 2. plot_binary_feature_rates

#' Plot binary feature rates (DLC / demo) by free/paid class
#'
#' Computes the proportion of games that have each binary feature
#' (columns whose names are passed in `feature_cols`) for each class,
#' then displays a dodged bar chart with percentage labels.
#'
#' @param df A data frame containing `is_free` (factor with levels
#'   `"Free"` and `"Paid"`) and all columns named in `feature_cols`
#'   (logical vectors).
#' @param feature_cols A character vector of logical column names to
#'   summarise. Defaults to `c("has_dlc", "has_demo")`.
#' @param free_colour Hex colour string for the "Free" bars.
#'   Defaults to `"#0EA5E9"`.
#' @param paid_colour Hex colour string for the "Paid" bars.
#'   Defaults to `"#2563EB"`.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- data.frame(
#'   is_free  = factor(rep(c("Free", "Paid"), each = 5),
#'                     levels = c("Free", "Paid")),
#'   has_dlc  = c(FALSE, FALSE, TRUE, FALSE, FALSE,
#'                TRUE,  TRUE,  TRUE, FALSE, TRUE),
#'   has_demo = c(FALSE, TRUE,  FALSE, FALSE, TRUE,
#'                TRUE,  TRUE,  FALSE, TRUE,  TRUE)
#' )
#' plot_binary_feature_rates(df)
plot_binary_feature_rates <- function(df,
                                      feature_cols = c("has_dlc", "has_demo"),
                                      free_colour  = "#0EA5E9",
                                      paid_colour  = "#2563EB") {
  stopifnot(
    is.data.frame(df),
    "is_free" %in% names(df),
    all(feature_cols %in% names(df)),
    length(feature_cols) >= 1,
    is.character(free_colour), length(free_colour) == 1,
    is.character(paid_colour), length(paid_colour) == 1
  )

  rates <- df |>
    group_by(is_free) |>
    summarise(
      across(all_of(feature_cols), mean, .names = "{.col}"),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols      = all_of(feature_cols),
      names_to  = "feature",
      values_to = "rate"
    )

  ggplot(rates, aes(x = feature, y = rate, fill = is_free)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_text(
      aes(label = percent(rate, accuracy = 0.1)),
      position = position_dodge(width = 0.7),
      vjust    = -0.35,
      size     = 3.8
    ) +
    scale_fill_manual(
      values = c("Free" = free_colour, "Paid" = paid_colour)
    ) +
    scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
    labs(
      title = "Binary Feature Rates by Class",
      x     = "Feature",
      y     = "Rate",
      fill  = "Class"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}


# 3. compute_category_gap 

#' Compute per-class category prevalence and the free-minus-paid gap
#'
#' For each `cat_*` indicator column present in `df`, calculates the
#' proportion of free and paid games that have the category, then
#' returns a tidy data frame with a `diff_free_minus_paid` column
#' sorted by absolute difference (largest first).
#'
#' This is a pure data-transformation function with no side-effects,
#' making it straightforward to test independently of any plotting code.
#'
#' @param df A data frame containing `is_free` (factor) and one or more
#'   logical columns whose names begin with `"cat_"`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{category}{Character. The `cat_*` column name.}
#'     \item{Free}{Numeric. Proportion of free games with this category.}
#'     \item{Paid}{Numeric. Proportion of paid games with this category.}
#'     \item{diff_free_minus_paid}{Numeric. Free proportion minus paid
#'       proportion.}
#'   }
#'   Rows are sorted by `abs(diff_free_minus_paid)` in descending order.
#'
#' @examples
#' df <- data.frame(
#'   is_free          = factor(c("Free","Free","Paid","Paid"),
#'                             levels = c("Free","Paid")),
#'   cat_single_player = c(FALSE, TRUE, TRUE, TRUE),
#'   cat_multi_player  = c(TRUE,  FALSE, FALSE, FALSE)
#' )
#' compute_category_gap(df)
compute_category_gap <- function(df) {
  stopifnot(is.data.frame(df), "is_free" %in% names(df))

  cat_cols <- grep("^cat_", names(df), value = TRUE)

  if (length(cat_cols) == 0) {
    stop("`df` must contain at least one column whose name starts with 'cat_'.")
  }

  class_totals <- df |>
    dplyr::count(is_free, name = "class_total")

  out <- df |>
    dplyr::select(is_free, dplyr::all_of(cat_cols)) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(cat_cols),
      names_to  = "category",
      values_to = "present"
    ) |>
    dplyr::filter(present) |>
    dplyr::count(is_free, category) |>
    dplyr::left_join(class_totals, by = "is_free") |>
    dplyr::mutate(rate = n / class_total) |>
    dplyr::select(is_free, category, rate) |>
    tidyr::pivot_wider(
      names_from  = is_free,
      values_from = rate,
      values_fill = 0
    )

  # ⭐ THE CRUCIAL TEST REQUIREMENT ⭐
  # Ensure both columns exist even if one class is missing
  if (!"Free" %in% names(out)) out$Free <- 0
  if (!"Paid" %in% names(out)) out$Paid <- 0

  out |>
    dplyr::mutate(diff_free_minus_paid = Free - Paid) |>
    dplyr::arrange(dplyr::desc(abs(diff_free_minus_paid)))
}


# 4. plot_category_gap 

#' Bar chart of the largest free-vs-paid category prevalence gaps
#'
#' Takes the output of [compute_category_gap()] (or any data frame with
#' the same structure) and renders a horizontal bar chart showing the
#' `top_n` categories with the largest absolute difference between free
#' and paid prevalence rates.
#'
#' @param cat_gap A data frame as returned by [compute_category_gap()],
#'   containing columns `category` (character), `Free` (numeric),
#'   `Paid` (numeric), and `diff_free_minus_paid` (numeric).
#' @param top_n A single positive integer giving the number of categories
#'   to display. Defaults to `12`.
#' @param free_colour Hex colour string for bars where free > paid.
#'   Defaults to `"#0EA5E9"`.
#' @param paid_colour Hex colour string for bars where paid > free.
#'   Defaults to `"#2563EB"`.
#'
#' @return A ggplot object.
#'
#' @examples
#' gap <- data.frame(
#'   category            = c("cat_single_player", "cat_multi_player"),
#'   Free                = c(0.30, 0.60),
#'   Paid                = c(0.55, 0.35),
#'   diff_free_minus_paid = c(-0.25, 0.25)
#' )
#' plot_category_gap(gap, top_n = 2)
plot_category_gap <- function(cat_gap,
                               top_n       = 12,
                               free_colour = "#0EA5E9",
                               paid_colour = "#2563EB") {
  stopifnot(
    is.data.frame(cat_gap),
    all(c("category", "diff_free_minus_paid") %in% names(cat_gap)),
    is.numeric(top_n), length(top_n) == 1, top_n >= 1,
    is.character(free_colour), length(free_colour) == 1,
    is.character(paid_colour), length(paid_colour) == 1
  )

  plot_data <- cat_gap |>
    slice_head(n = as.integer(top_n))

  ggplot(
    plot_data,
    aes(
      x    = reorder(category, diff_free_minus_paid),
      y    = diff_free_minus_paid,
      fill = diff_free_minus_paid > 0
    )
  ) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(
      values = c("TRUE" = free_colour, "FALSE" = paid_colour),
      labels = c("FALSE" = "Higher for Paid", "TRUE" = "Higher for Free"),
      name   = "Direction"
    ) +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Largest Class Differences in Top Category Prevalence",
      x     = "Category",
      y     = "Free Rate \u2212 Paid Rate"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}
