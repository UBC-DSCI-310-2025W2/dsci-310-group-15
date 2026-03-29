library(testthat)
library(tidyverse)
library(scales)

source(here::here("R", "plot_functions.R"))

# ============================================================
# Shared fixtures
# ============================================================

# A minimal data frame that all four functions can consume.
# group_col = "group", year_col = "year",
# flag columns = "flag_a", "flag_b",
# indicator columns = "indicator_x", "indicator_y"
make_df <- function() {
  data.frame(
    group       = factor(
      c("A", "A", "A", "A", "B", "B", "B", "B", "A", "B"),
      levels = c("A", "B")
    ),
    year        = c(2018, 2019, 2020, 2021, 2018, 2019, 2020, 2021, -1, 2022),
    flag_a      = c(FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, TRUE),
    flag_b      = c(TRUE,  FALSE, FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE),
    indicator_x = c(TRUE,  TRUE,  FALSE, TRUE,  TRUE,  TRUE,  FALSE, TRUE,  TRUE,  FALSE),
    indicator_y = c(FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, TRUE)
  )
}

# A pre-computed gap data frame matching compute_indicator_gap() output.
make_gap_df <- function() {
  data.frame(
    indicator = c("indicator_x", "indicator_y", "indicator_z"),
    A         = c(0.75, 0.50, 0.20),
    B         = c(0.50, 0.60, 0.10),
    diff      = c(0.25, -0.10, 0.10)
  )
}


# ============================================================
# 1. plot_counts_over_time_by_group
# ============================================================

# --- Normal cases ---

test_that(
  "plot_counts_over_time_by_group returns a ggplot object for a well-formed input", {
    p <- plot_counts_over_time_by_group(make_df(),
                                        year_col  = "year",
                                        group_col = "group")
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "plot_counts_over_time_by_group maps year_col to the x aesthetic", {
    p <- plot_counts_over_time_by_group(make_df(),
                                        year_col  = "year",
                                        group_col = "group")
    expect_equal(as.character(p$mapping$x), "~.data[[year_col]]")
  }
)

test_that(
  "plot_counts_over_time_by_group applies custom colour values when supplied", {
    colours <- c("A" = "#111111", "B" = "#222222")
    p       <- plot_counts_over_time_by_group(make_df(),
                                              year_col  = "year",
                                              group_col = "group",
                                              colours   = colours)
    colour_scale <- Filter(
      function(s) "colour" %in% s$aesthetics,
      p$scales$scales
    )[[1]]
    expect_equal(colour_scale$palette(2), c("#111111", "#222222"))
  }
)

test_that(
  "plot_counts_over_time_by_group applies custom axis labels and title", {
    p <- plot_counts_over_time_by_group(make_df(),
                                        year_col  = "year",
                                        group_col = "group",
                                        x_label   = "My X",
                                        y_label   = "My Y",
                                        title     = "My Title")
    expect_equal(p$labels$x,     "My X")
    expect_equal(p$labels$y,     "My Y")
    expect_equal(p$labels$title, "My Title")
  }
)

# --- Edge cases ---

test_that(
  "plot_counts_over_time_by_group silently drops rows where year_col is non-positive", {
    df   <- make_df()   # contains one row with year == -1
    p    <- plot_counts_over_time_by_group(df, year_col = "year", group_col = "group")
    data <- layer_data(p)
    expect_true(all(data$x > 0))
  }
)

test_that(
  "plot_counts_over_time_by_group works when colours is NULL and lets ggplot choose", {
    p <- plot_counts_over_time_by_group(make_df(),
                                        year_col  = "year",
                                        group_col = "group",
                                        colours   = NULL)
    expect_s3_class(p, "ggplot")
    # No explicit colour scale should have been added
    colour_scales <- Filter(
      function(s) "colour" %in% s$aesthetics,
      p$scales$scales
    )
    expect_length(colour_scales, 0)
  }
)

# --- Error cases ---

test_that(
  "plot_counts_over_time_by_group throws an error when year_col is absent from df", {
    bad_df <- make_df() |> select(-year)
    expect_error(
      plot_counts_over_time_by_group(bad_df, year_col = "year", group_col = "group")
    )
  }
)

test_that(
  "plot_counts_over_time_by_group throws an error when group_col is absent from df", {
    bad_df <- make_df() |> select(-group)
    expect_error(
      plot_counts_over_time_by_group(bad_df, year_col = "year", group_col = "group")
    )
  }
)

test_that(
  "plot_counts_over_time_by_group throws an error when df is not a data frame", {
    expect_error(
      plot_counts_over_time_by_group(list(year = 1, group = "A"),
                                     year_col  = "year",
                                     group_col = "group")
    )
  }
)

test_that(
  "plot_counts_over_time_by_group throws an error when year_col is not a string", {
    expect_error(
      plot_counts_over_time_by_group(make_df(), year_col = 1, group_col = "group")
    )
  }
)


# ============================================================
# 2. plot_binary_rates_by_group
# ============================================================

# --- Normal cases ---

test_that(
  "plot_binary_rates_by_group returns a ggplot object for a well-formed input", {
    p <- plot_binary_rates_by_group(make_df(),
                                    group_col = "group",
                                    flag_cols = c("flag_a", "flag_b"))
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "plot_binary_rates_by_group produces one bar per group level per flag column", {
    p    <- plot_binary_rates_by_group(make_df(),
                                       group_col = "group",
                                       flag_cols = c("flag_a", "flag_b"))
    data <- layer_data(p)
    # 2 flag columns × 2 group levels = 4 bars
    expect_equal(nrow(data), 4L)
  }
)

test_that(
  "plot_binary_rates_by_group computes rates bounded between 0 and 1", {
    p    <- plot_binary_rates_by_group(make_df(),
                                       group_col = "group",
                                       flag_cols = c("flag_a", "flag_b"))
    data <- layer_data(p)
    expect_true(all(data$y >= 0 & data$y <= 1))
  }
)

test_that(
  "plot_binary_rates_by_group accepts a single flag column without error", {
    p    <- plot_binary_rates_by_group(make_df(),
                                       group_col = "group",
                                       flag_cols = "flag_a")
    data <- layer_data(p)
    # 1 flag × 2 group levels = 2 bars
    expect_equal(nrow(data), 2L)
  }
)

test_that(
  "plot_binary_rates_by_group applies custom colour values when supplied", {
    colours <- c("A" = "#AABBCC", "B" = "#DDEEFF")
    p       <- plot_binary_rates_by_group(make_df(),
                                          group_col = "group",
                                          flag_cols = c("flag_a", "flag_b"),
                                          colours   = colours)
    fill_scale <- Filter(
      function(s) "fill" %in% s$aesthetics,
      p$scales$scales
    )[[1]]
    expect_true("#AABBCC" %in% fill_scale$palette(2))
  }
)

# --- Edge cases ---

test_that(
  "plot_binary_rates_by_group works when colours is NULL and lets ggplot choose", {
    p <- plot_binary_rates_by_group(make_df(),
                                    group_col = "group",
                                    flag_cols = c("flag_a", "flag_b"),
                                    colours   = NULL)
    expect_s3_class(p, "ggplot")
    fill_scales <- Filter(
      function(s) "fill" %in% s$aesthetics,
      p$scales$scales
    )
    expect_length(fill_scales, 0)
  }
)

# --- Error cases ---

test_that(
  "plot_binary_rates_by_group throws an error when a flag column is absent from df", {
    expect_error(
      plot_binary_rates_by_group(make_df(),
                                 group_col = "group",
                                 flag_cols = c("flag_a", "nonexistent"))
    )
  }
)

test_that(
  "plot_binary_rates_by_group throws an error when group_col is absent from df", {
    bad_df <- make_df() |> select(-group)
    expect_error(
      plot_binary_rates_by_group(bad_df,
                                 group_col = "group",
                                 flag_cols = "flag_a")
    )
  }
)

test_that(
  "plot_binary_rates_by_group throws an error when flag_cols is an empty vector", {
    expect_error(
      plot_binary_rates_by_group(make_df(),
                                 group_col = "group",
                                 flag_cols = character(0))
    )
  }
)

test_that(
  "plot_binary_rates_by_group throws an error when df is not a data frame", {
    expect_error(
      plot_binary_rates_by_group("not a data frame",
                                 group_col = "group",
                                 flag_cols = "flag_a")
    )
  }
)


# ============================================================
# 3. compute_indicator_gap
# ============================================================

# --- Normal cases ---

test_that(
  "compute_indicator_gap returns a data frame", {
    result <- compute_indicator_gap(make_df(),
                                    group_col   = "group",
                                    col_pattern = "^indicator_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_s3_class(result, "data.frame")
  }
)

test_that(
  "compute_indicator_gap output contains the required columns", {
    result <- compute_indicator_gap(make_df(),
                                    group_col   = "group",
                                    col_pattern = "^indicator_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_true(all(c("indicator", "A", "B", "diff") %in% names(result)))
  }
)

test_that(
  "compute_indicator_gap returns one row per matched indicator column", {
    result     <- compute_indicator_gap(make_df(),
                                        group_col   = "group",
                                        col_pattern = "^indicator_",
                                        level_a     = "A",
                                        level_b     = "B")
    n_expected <- sum(grepl("^indicator_", names(make_df())))
    expect_equal(nrow(result), n_expected)
  }
)

test_that(
  "compute_indicator_gap diff column equals level_a rate minus level_b rate", {
    result <- compute_indicator_gap(make_df(),
                                    group_col   = "group",
                                    col_pattern = "^indicator_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_equal(result$diff, result$A - result$B)
  }
)

test_that(
  "compute_indicator_gap rows are sorted by absolute diff in descending order", {
    result <- compute_indicator_gap(make_df(),
                                    group_col   = "group",
                                    col_pattern = "^indicator_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_true(all(diff(abs(result$diff)) <= 0))
  }
)

test_that(
  "compute_indicator_gap rates are bounded between 0 and 1 inclusive", {
    result <- compute_indicator_gap(make_df(),
                                    group_col   = "group",
                                    col_pattern = "^indicator_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_true(all(result$A >= 0 & result$A <= 1))
    expect_true(all(result$B >= 0 & result$B <= 1))
  }
)

# --- Edge cases ---

test_that(
  "compute_indicator_gap fills rate with 0 when a group has no TRUE values for an indicator", {
    df_one_sided <- data.frame(
      group       = factor(c("A", "B", "B"), levels = c("A", "B")),
      indicator_x = c(FALSE, TRUE, TRUE)
    )
    result <- compute_indicator_gap(df_one_sided,
                                    group_col   = "group",
                                    col_pattern = "^indicator_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_equal(result$A[result$indicator == "indicator_x"], 0)
  }
)

test_that(
  "compute_indicator_gap works with a custom col_pattern matching a different prefix", {
    df <- make_df() |>
      rename(feat_x = indicator_x, feat_y = indicator_y)
    result <- compute_indicator_gap(df,
                                    group_col   = "group",
                                    col_pattern = "^feat_",
                                    level_a     = "A",
                                    level_b     = "B")
    expect_equal(nrow(result), 2L)
  }
)

# --- Error cases ---

test_that(
  "compute_indicator_gap throws an error when no columns match col_pattern", {
    expect_error(
      compute_indicator_gap(make_df(),
                            group_col   = "group",
                            col_pattern = "^nomatch_",
                            level_a     = "A",
                            level_b     = "B")
    )
  }
)

test_that(
  "compute_indicator_gap throws an error when group_col is absent from df", {
    bad_df <- make_df() |> select(-group)
    expect_error(
      compute_indicator_gap(bad_df,
                            group_col   = "group",
                            col_pattern = "^indicator_",
                            level_a     = "A",
                            level_b     = "B")
    )
  }
)

test_that(
  "compute_indicator_gap throws an error when df is not a data frame", {
    expect_error(
      compute_indicator_gap("not a data frame",
                            group_col   = "group",
                            col_pattern = "^indicator_",
                            level_a     = "A",
                            level_b     = "B")
    )
  }
)


# ============================================================
# 4. plot_indicator_gap
# ============================================================

# --- Normal cases ---

test_that(
  "plot_indicator_gap returns a ggplot object for a well-formed gap data frame", {
    p <- plot_indicator_gap(make_gap_df())
    expect_s3_class(p, "ggplot")
  }
)

test_that(
  "plot_indicator_gap respects top_n and only shows that many bars", {
    p    <- plot_indicator_gap(make_gap_df(), top_n = 2)
    data <- layer_data(p)
    expect_equal(nrow(data), 2L)
  }
)

test_that(
  "plot_indicator_gap applies coord_flip so bars are horizontal", {
    p <- plot_indicator_gap(make_gap_df())
    expect_true(inherits(p$coordinates, "CoordFlip"))
  }
)

test_that(
  "plot_indicator_gap applies custom positive and negative colours", {
    p <- plot_indicator_gap(make_gap_df(),
                             pos_colour = "#111111",
                             neg_colour = "#222222")
    fill_scale <- Filter(
      function(s) "fill" %in% s$aesthetics,
      p$scales$scales
    )[[1]]
    palette_vals <- fill_scale$palette(2)
    expect_true("#111111" %in% palette_vals)
    expect_true("#222222" %in% palette_vals)
  }
)

test_that(
  "plot_indicator_gap applies custom axis label and title", {
    p <- plot_indicator_gap(make_gap_df(),
                             x_label = "Rate difference",
                             title   = "My gap chart")
    expect_equal(p$labels$y,     "Rate difference")
    expect_equal(p$labels$title, "My gap chart")
  }
)

# --- Edge cases ---

test_that(
  "plot_indicator_gap still works when top_n equals the number of rows in gap_df", {
    p    <- plot_indicator_gap(make_gap_df(), top_n = nrow(make_gap_df()))
    data <- layer_data(p)
    expect_equal(nrow(data), nrow(make_gap_df()))
  }
)

test_that(
  "plot_indicator_gap still works when top_n exceeds the number of rows in gap_df", {
    p    <- plot_indicator_gap(make_gap_df(), top_n = 100)
    data <- layer_data(p)
    expect_equal(nrow(data), nrow(make_gap_df()))
  }
)

# --- Error cases ---

test_that(
  "plot_indicator_gap throws an error when the required 'diff' column is missing", {
    bad <- make_gap_df() |> select(-diff)
    expect_error(plot_indicator_gap(bad))
  }
)

test_that(
  "plot_indicator_gap throws an error when the required 'indicator' column is missing", {
    bad <- make_gap_df() |> select(-indicator)
    expect_error(plot_indicator_gap(bad))
  }
)

test_that(
  "plot_indicator_gap throws an error when top_n is zero or negative", {
    expect_error(plot_indicator_gap(make_gap_df(), top_n = 0))
    expect_error(plot_indicator_gap(make_gap_df(), top_n = -1))
  }
)

test_that(
  "plot_indicator_gap throws an error when top_n is not numeric", {
    expect_error(plot_indicator_gap(make_gap_df(), top_n = "all"))
  }
)

test_that(
  "plot_indicator_gap throws an error when gap_df is not a data frame", {
    expect_error(plot_indicator_gap("not a data frame"))
  }
)
