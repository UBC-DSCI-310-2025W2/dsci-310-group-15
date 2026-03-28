library(testthat)
library(tidyverse)
library(scales)
library(patchwork)

source("../../R/extract_values.R")


make_df <- function() {
  data.frame(
    game_id           = 1:10,
    is_free           = factor(
      c("Free", "Free", "Free", "Paid", "Paid",
        "Paid", "Paid", "Paid", "Free", "Paid"),
      levels = c("Free", "Paid")
    ),
    release_year      = c(2018, 2019, 2020, 2018, 2019,
                          2020, 2021, 2021, -1,   2022),
    has_dlc           = c(FALSE, FALSE, TRUE,  TRUE, TRUE,
                          TRUE,  TRUE,  FALSE, FALSE, TRUE),
    has_demo          = c(TRUE,  FALSE, FALSE, TRUE, FALSE,
                          TRUE,  TRUE,  FALSE, FALSE, TRUE),
    cat_single_player = c(TRUE,  TRUE,  FALSE, TRUE, TRUE,
                          TRUE,  FALSE, TRUE,  TRUE,  FALSE),
    cat_multi_player  = c(FALSE, TRUE,  TRUE,  FALSE, FALSE,
                          FALSE, TRUE,  FALSE, FALSE, TRUE)
  )
}


# 1. plot_release_year_by_class

test_that("plot_release_year_by_class returns a ggplot", {
  p <- plot_release_year_by_class(make_df())
  expect_s3_class(p, "ggplot")
})

test_that("plot_release_year_by_class excludes non-positive release years", {
  p    <- plot_release_year_by_class(make_df())
  data <- layer_data(p)
  # release_year == -1 row should not appear in the plot data
  expect_true(all(data$x > 0))
})


test_that("plot_release_year_by_class uses custom colours", {
  p      <- plot_release_year_by_class(make_df(),
                                       free_colour = "#AAAAAA",
                                       paid_colour = "#BBBBBB")
  scales <- p$scales$scales
  colour_scale <- Filter(function(s) "colour" %in% s$aesthetics, scales)[[1]]
  expect_equal(unname(colour_scale$palette(2)),
               c("#AAAAAA", "#BBBBBB"))
})

test_that("plot_release_year_by_class errors on missing column", {
  bad <- make_df() |> select(-release_year)
  expect_error(plot_release_year_by_class(bad))
})

test_that("plot_release_year_by_class errors when df is not a data frame", {
  expect_error(plot_release_year_by_class(list(release_year = 1, is_free = "x")))
})

test_that("plot_release_year_by_class errors on non-string colour", {
  expect_error(plot_release_year_by_class(make_df(), free_colour = 123))
})


# 2. plot_binary_feature_rates

test_that("plot_binary_feature_rates returns a ggplot", {
  p <- plot_binary_feature_rates(make_df())
  expect_s3_class(p, "ggplot")
})

test_that("plot_binary_feature_rates includes one bar per class per feature", {
  p    <- plot_binary_feature_rates(make_df())
  data <- layer_data(p)
  # 2 features × 2 classes = 4 bars
  expect_equal(nrow(data), 4L)
})

test_that("plot_binary_feature_rates rates are between 0 and 1", {
  p    <- plot_binary_feature_rates(make_df())
  data <- layer_data(p)
  expect_true(all(data$y >= 0 & data$y <= 1))
})

test_that("plot_binary_feature_rates accepts custom feature columns", {
  p <- plot_binary_feature_rates(make_df(), feature_cols = "has_dlc")
  expect_s3_class(p, "ggplot")
  data <- layer_data(p)
  expect_equal(nrow(data), 2L)   # 1 feature × 2 classes
})

test_that("plot_binary_feature_rates errors when feature column is absent", {
  expect_error(
    plot_binary_feature_rates(make_df(), feature_cols = "nonexistent_col")
  )
})

test_that("plot_binary_feature_rates errors on missing is_free column", {
  bad <- make_df() |> select(-is_free)
  expect_error(plot_binary_feature_rates(bad))
})

test_that("plot_binary_feature_rates errors when feature_cols is empty", {
  expect_error(
    plot_binary_feature_rates(make_df(), feature_cols = character(0))
  )
})


# 3. compute_category_gap

test_that("compute_category_gap returns a data frame", {
  result <- compute_category_gap(make_df())
  expect_s3_class(result, "data.frame")
})

test_that("compute_category_gap has the expected columns", {
  result <- compute_category_gap(make_df())
  expect_true(all(c("category", "Free", "Paid", "diff_free_minus_paid") %in% names(result)))
})

test_that("compute_category_gap has one row per cat_ column", {
  result <- compute_category_gap(make_df())
  n_cat_cols <- sum(grepl("^cat_", names(make_df())))
  expect_equal(nrow(result), n_cat_cols)
})

test_that("compute_category_gap diff equals Free minus Paid", {
  result <- compute_category_gap(make_df())
  expect_equal(result$diff_free_minus_paid, result$Free - result$Paid)
})

test_that("compute_category_gap is sorted by absolute diff descending", {
  result <- compute_category_gap(make_df())
  expect_true(all(diff(abs(result$diff_free_minus_paid)) <= 0))
})

test_that("compute_category_gap rates are between 0 and 1", {
  result <- compute_category_gap(make_df())
  expect_true(all(result$Free >= 0 & result$Free <= 1))
  expect_true(all(result$Paid >= 0 & result$Paid <= 1))
})

test_that("compute_category_gap errors when no cat_ columns present", {
  bad <- make_df() |> select(-starts_with("cat_"))
  expect_error(compute_category_gap(bad))
})

test_that("compute_category_gap errors when is_free is missing", {
  bad <- make_df() |> select(-is_free)
  expect_error(compute_category_gap(bad))
})

test_that("compute_category_gap errors on non-data-frame input", {
  expect_error(compute_category_gap("not a data frame"))
})

test_that("compute_category_gap fills missing class combinations with 0", {
  # A category present only in paid games should have Free = 0
  df_one_sided <- data.frame(
    is_free          = factor(c("Free", "Paid", "Paid"),
                              levels = c("Free", "Paid")),
    cat_only_paid    = c(FALSE, TRUE, TRUE)
  )
  result <- compute_category_gap(df_one_sided)
  expect_equal(result$Free[result$category == "cat_only_paid"], 0)
})


# 4. plot_category_gap

make_gap <- function() {
  compute_category_gap(make_df())
}

test_that("plot_category_gap returns a ggplot", {
  p <- plot_category_gap(make_gap())
  expect_s3_class(p, "ggplot")
})

test_that("plot_category_gap respects top_n", {
  p    <- plot_category_gap(make_gap(), top_n = 1)
  data <- layer_data(p)
  expect_equal(nrow(data), 1L)
})

test_that("plot_category_gap has coord_flip applied", {
  p <- plot_category_gap(make_gap())
  expect_true(inherits(p$coordinates, "CoordFlip"))
})

test_that("plot_category_gap uses custom colours", {
  p      <- plot_category_gap(make_gap(),
                               free_colour = "#111111",
                               paid_colour = "#222222")
  scales <- p$scales$scales
  fill_scale <- Filter(function(s) "fill" %in% s$aesthetics, scales)[[1]]
  palette_vals <- fill_scale$palette(2)
  expect_true("#111111" %in% palette_vals)
  expect_true("#222222" %in% palette_vals)
})

test_that("plot_category_gap errors on missing required columns", {
  bad <- make_gap() |> select(-diff_free_minus_paid)
  expect_error(plot_category_gap(bad))
})

test_that("plot_category_gap errors when top_n is not a positive number", {
  expect_error(plot_category_gap(make_gap(), top_n = 0))
  expect_error(plot_category_gap(make_gap(), top_n = -1))
})

test_that("plot_category_gap errors when top_n is not numeric", {
  expect_error(plot_category_gap(make_gap(), top_n = "all"))
})

test_that("plot_category_gap errors on non-data-frame input", {
  expect_error(plot_category_gap("not a data frame"))
})
