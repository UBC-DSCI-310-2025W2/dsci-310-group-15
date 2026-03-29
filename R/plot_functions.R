library(tidyverse)
library(scales)
library(patchwork)


# ---- 1. plot_counts_over_time_by_group ---------------------------------------

#' Plot observation counts over a numeric year column, split by a grouping variable
#'
#' Creates a line chart showing how many observations fall in each year,
#' with one line per level of the grouping variable. Rows where `year_col`
#' is non-positive are treated as missing and excluded before counting.
#'
#' @param df A data frame.
#' @param year_col A length-1 character string naming the numeric column
#'   that represents the year (or any ordered integer axis).
#' @param group_col A length-1 character string naming the factor or
#'   character column used to colour the lines.
#' @param colours A named character vector mapping each level of `group_col`
#'   to a hex colour. If `NULL` (default), ggplot2 selects colours
#'   automatically.
#' @param x_label A length-1 character string for the x-axis label.
#'   Defaults to the value of `year_col`.
#' @param y_label A length-1 character string for the y-axis label.
#'   Defaults to `"Count"`.
#' @param title A length-1 character string for the plot title.
#'   Defaults to `""` (no title).
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- data.frame(
#'   year  = c(2018, 2019, 2019, 2020, 2020, 2020),
#'   group = factor(c("A", "A", "B", "A", "B", "B"),
#'                  levels = c("A", "B"))
#' )
#' plot_counts_over_time_by_group(df, year_col = "year", group_col = "group")
plot_counts_over_time_by_group <- function(df,
                                           year_col,
                                           group_col,
                                           colours = NULL,
                                           x_label = year_col,
                                           y_label = "Count",
                                           title   = "") {
  stopifnot(
    is.data.frame(df),
    is.character(year_col),  length(year_col)  == 1, year_col  %in% names(df),
    is.character(group_col), length(group_col) == 1, group_col %in% names(df),
    is.character(x_label),   length(x_label)   == 1,
    is.character(y_label),   length(y_label)   == 1,
    is.character(title),     length(title)     == 1
  )

  counts <- df |>
    filter(.data[[year_col]] > 0) |>
    count(.data[[year_col]], .data[[group_col]])

  p <- ggplot(
    counts,
    aes(
      x     = .data[[year_col]],
      y     = n,
      color = .data[[group_col]]
    )
  ) +
    geom_line(linewidth = 1) +
    labs(
      title = title,
      x     = x_label,
      y     = y_label,
      color = group_col
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))

  if (!is.null(colours)) {
    p <- p + scale_color_manual(values = colours)
  }

  p
}


# ---- 2. plot_binary_rates_by_group -------------------------------------------

#' Plot the proportion of TRUE values in logical columns, split by a grouping variable
#'
#' For each column listed in `flag_cols`, computes the proportion of rows
#' where the value is `TRUE` within each level of `group_col`, then
#' displays a dodged bar chart with percentage labels above each bar.
#' This is useful for comparing feature prevalence across classes.
#'
#' @param df A data frame.
#' @param group_col A length-1 character string naming the grouping column
#'   (factor or character). One set of bars is drawn per level.
#' @param flag_cols A character vector of column names. Each column must
#'   be coercible to logical (i.e., contain only `TRUE`/`FALSE`/`NA`).
#'   Must have at least one element.
#' @param colours A named character vector mapping each level of `group_col`
#'   to a hex colour. If `NULL` (default), ggplot2 selects colours
#'   automatically.
#' @param x_label A length-1 character string for the x-axis label.
#'   Defaults to `"Feature"`.
#' @param y_label A length-1 character string for the y-axis label.
#'   Defaults to `"Proportion"`.
#' @param title A length-1 character string for the plot title.
#'   Defaults to `""`.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- data.frame(
#'   group    = factor(rep(c("A", "B"), each = 4),
#'                     levels = c("A", "B")),
#'   flag_one = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
#'   flag_two = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
#' )
#' plot_binary_rates_by_group(df,
#'                            group_col = "group",
#'                            flag_cols = c("flag_one", "flag_two"))
plot_binary_rates_by_group <- function(df,
                                       group_col,
                                       flag_cols,
                                       colours = NULL,
                                       x_label = "Feature",
                                       y_label = "Proportion",
                                       title   = "") {
  stopifnot(
    is.data.frame(df),
    is.character(group_col), length(group_col) == 1, group_col %in% names(df),
    is.character(flag_cols), length(flag_cols) >= 1,
    all(flag_cols %in% names(df)),
    is.character(x_label), length(x_label) == 1,
    is.character(y_label), length(y_label) == 1,
    is.character(title),   length(title)   == 1
  )

  rates <- df |>
    group_by(.data[[group_col]]) |>
    summarise(
      across(all_of(flag_cols), \(x) mean(as.logical(x), na.rm = TRUE)),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols      = all_of(flag_cols),
      names_to  = "feature",
      values_to = "rate"
    )

  p <- ggplot(rates, aes(x = feature, y = rate, fill = .data[[group_col]])) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    geom_text(
      aes(label = percent(rate, accuracy = 0.1)),
      position = position_dodge(width = 0.7),
      vjust    = -0.35,
      size     = 3.8
    ) +
    scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
    labs(
      title = title,
      x     = x_label,
      y     = y_label,
      fill  = group_col
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))

  if (!is.null(colours)) {
    p <- p + scale_fill_manual(values = colours)
  }

  p
}


# ---- 3. compute_indicator_gap ------------------------------------------------

#' Compute per-group prevalence rates and the pairwise difference for indicator columns
#'
#' For each indicator (logical) column whose name matches `col_pattern`,
#' computes the proportion of `TRUE` values within each level of `group_col`,
#' then pivots the result wide and adds a difference column
#' (`<level_a> - <level_b>`). Rows are sorted by absolute difference,
#' largest first.
#'
#' This is a pure data-transformation function with no side-effects and
#' is therefore straightforward to unit-test independently of any
#' plotting code.
#'
#' @param df A data frame.
#' @param group_col A length-1 character string naming the two-level
#'   grouping column (factor or character).
#' @param col_pattern A length-1 character string containing a regular
#'   expression used to select indicator columns from `df`. Matched
#'   against `names(df)` via [grep()]. Defaults to `"^indicator_"`.
#' @param level_a A length-1 character string giving the first group
#'   level. The difference is computed as `rate(level_a) - rate(level_b)`.
#' @param level_b A length-1 character string giving the second group
#'   level.
#'
#' @return A tibble with one row per matched indicator column and columns:
#'   \describe{
#'     \item{indicator}{Character. The matched column name.}
#'     \item{<level_a>}{Numeric. Proportion of `TRUE` values for
#'       `level_a`.}
#'     \item{<level_b>}{Numeric. Proportion of `TRUE` values for
#'       `level_b`.}
#'     \item{diff}{Numeric. `rate(level_a) - rate(level_b)`.}
#'   }
#'   Sorted by `abs(diff)` descending.
#'
#' @examples
#' df <- data.frame(
#'   group      = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
#'   indicator_x = c(TRUE,  FALSE, TRUE, TRUE),
#'   indicator_y = c(FALSE, FALSE, TRUE, FALSE)
#' )
#' compute_indicator_gap(df,
#'                       group_col   = "group",
#'                       col_pattern = "^indicator_",
#'                       level_a     = "A",
#'                       level_b     = "B")
compute_indicator_gap <- function(df,
                                  group_col,
                                  col_pattern = "^indicator_",
                                  level_a,
                                  level_b) {
  stopifnot(
    is.data.frame(df),
    is.character(group_col),   length(group_col)   == 1, group_col %in% names(df),
    is.character(col_pattern), length(col_pattern) == 1,
    is.character(level_a),     length(level_a)     == 1,
    is.character(level_b),     length(level_b)     == 1
  )

  ind_cols <- grep(col_pattern, names(df), value = TRUE)

  if (length(ind_cols) == 0) {
    stop(
      paste0(
        "No columns in `df` match the pattern '", col_pattern, "'. ",
        "Check `col_pattern` or column names."
      )
    )
  }

  group_totals <- df |>
    count(.data[[group_col]], name = "group_total")

  df |>
    select(all_of(c(group_col, ind_cols))) |>
    pivot_longer(
      cols      = all_of(ind_cols),
      names_to  = "indicator",
      values_to = "present"
    ) |>
    filter(as.logical(present)) |>
    count(.data[[group_col]], indicator) |>
    left_join(group_totals, by = group_col) |>
    mutate(rate = n / group_total) |>
    select(all_of(group_col), indicator, rate) |>
    pivot_wider(
      names_from  = all_of(group_col),
      values_from = rate,
      values_fill = 0
    ) |>
    mutate(diff = .data[[level_a]] - .data[[level_b]]) |>
    arrange(desc(abs(diff)))
}


# ---- 4. plot_indicator_gap ---------------------------------------------------

#' Horizontal bar chart of the largest between-group indicator prevalence gaps
#'
#' Visualises the output of [compute_indicator_gap()] (or any data frame
#' with a `diff` column and an `indicator` column) as a horizontal bar
#' chart. Bars pointing in the positive direction are coloured differently
#' from bars pointing in the negative direction, making it easy to see
#' which indicators favour each group.
#'
#' @param gap_df A data frame containing at minimum the columns `indicator`
#'   (character) and `diff` (numeric), as returned by
#'   [compute_indicator_gap()].
#' @param top_n A single positive integer giving the maximum number of
#'   indicators to display. Rows are taken from the top of `gap_df` (i.e.,
#'   the caller is responsible for sorting). Defaults to `10`.
#' @param pos_colour Hex colour string for bars where `diff > 0`.
#'   Defaults to `"#0EA5E9"`.
#' @param neg_colour Hex colour string for bars where `diff <= 0`.
#'   Defaults to `"#2563EB"`.
#' @param pos_label Legend label for bars where `diff > 0`.
#'   Defaults to `"Higher in group A"`.
#' @param neg_label Legend label for bars where `diff <= 0`.
#'   Defaults to `"Higher in group B"`.
#' @param x_label A length-1 character string for the x-axis label
#'   (after `coord_flip`, this appears on the horizontal axis).
#'   Defaults to `"Difference in prevalence"`.
#' @param title A length-1 character string for the plot title.
#'   Defaults to `""`.
#'
#' @return A ggplot object.
#'
#' @examples
#' gap <- data.frame(
#'   indicator = c("indicator_x", "indicator_y"),
#'   diff      = c(0.25, -0.15)
#' )
#' plot_indicator_gap(gap, top_n = 2)
plot_indicator_gap <- function(gap_df,
                                top_n     = 10,
                                pos_colour = "#0EA5E9",
                                neg_colour = "#2563EB",
                                pos_label  = "Higher in group A",
                                neg_label  = "Higher in group B",
                                x_label    = "Difference in prevalence",
                                title      = "") {
  stopifnot(
    is.data.frame(gap_df),
    all(c("indicator", "diff") %in% names(gap_df)),
    is.numeric(top_n),     length(top_n)     == 1, top_n >= 1,
    is.character(pos_colour), length(pos_colour) == 1,
    is.character(neg_colour), length(neg_colour) == 1,
    is.character(pos_label),  length(pos_label)  == 1,
    is.character(neg_label),  length(neg_label)  == 1,
    is.character(x_label),    length(x_label)    == 1,
    is.character(title),      length(title)      == 1
  )

  plot_data <- gap_df |>
    slice_head(n = as.integer(top_n)) |>
    mutate(direction = diff > 0)

  ggplot(
    plot_data,
    aes(
      x    = reorder(indicator, diff),
      y    = diff,
      fill = direction
    )
  ) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(
      values = c("TRUE" = pos_colour, "FALSE" = neg_colour),
      labels = c("TRUE" = pos_label,  "FALSE" = neg_label),
      name   = NULL
    ) +
    scale_y_continuous(labels = percent) +
    labs(
      title = title,
      x     = "Indicator",
      y     = x_label
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}
