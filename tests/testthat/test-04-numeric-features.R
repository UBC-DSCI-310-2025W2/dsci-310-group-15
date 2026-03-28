library(testthat)

make_toy_numeric_df <- function() {
  data.frame(
    is_free = factor(c("Free", "Paid", "Free"), levels = c("Free", "Paid")),
    required_age = c(0, 18, 13),
    release_year = c(2020, 2019, -1),
    platform_count = c(3, 1, 2),
    n_categories = c(6, 2, 4)
  )
}

test_that("pivot_numeric_predictors works for default predictors", {
  toy_df <- make_toy_numeric_df()

  out <- pivot_numeric_predictors(toy_df)

  expect_true(is.data.frame(out))
  expect_true(all(c("is_free", "predictor", "value") %in% names(out)))
  expect_true(all(unique(out$predictor) %in% c("required_age", "release_year", "platform_count", "n_categories")))
})

test_that("pivot_numeric_predictors supports custom predictor subsets", {
  toy_df <- make_toy_numeric_df()

  out <- pivot_numeric_predictors(toy_df, predictors = c("required_age", "platform_count"))

  expect_equal(sort(unique(out$predictor)), c("platform_count", "required_age"))
  expect_equal(nrow(out), nrow(toy_df) * 2)
})

test_that("pivot_numeric_predictors filters non-positive release_year values", {
  toy_df <- make_toy_numeric_df()

  out <- pivot_numeric_predictors(toy_df)
  bad_release_year <- out$predictor == "release_year" & out$value <= 0

  expect_false(any(bad_release_year))
})

test_that("pivot_numeric_predictors fails for non-numeric predictors", {
  toy_df <- make_toy_numeric_df()
  toy_df$required_age <- as.character(toy_df$required_age)

  expect_error(
    pivot_numeric_predictors(toy_df),
    "All predictors must be numeric"
  )
})

test_that("build_numeric_distribution_plot returns a ggplot object", {
  toy_long <- data.frame(
    is_free = factor(c("Free", "Free", "Paid", "Paid")),
    predictor = c("required_age", "platform_count", "required_age", "platform_count"),
    value = c(0, 3, 18, 1)
  )

  p <- build_numeric_distribution_plot(toy_long)
  expect_s3_class(p, "ggplot")
})

test_that("build_numeric_distribution_plot handles repeated values edge case", {
  toy_long <- data.frame(
    is_free = factor(rep(c("Free", "Paid"), each = 4)),
    predictor = rep(c("required_age", "platform_count"), times = 4),
    value = rep(1, 8)
  )

  p <- build_numeric_distribution_plot(toy_long)
  expect_s3_class(p, "ggplot")
})

test_that("build_numeric_distribution_plot fails with missing columns", {
  bad_long <- data.frame(
    is_free = factor(c("Free", "Paid")),
    predictor = c("required_age", "required_age")
  )

  expect_error(
    build_numeric_distribution_plot(bad_long),
    "missing required columns"
  )
})

test_that("save_numeric_distribution_outputs saves files in existing directories", {
  toy_long <- data.frame(
    is_free = factor(c("Free", "Free", "Paid", "Paid")),
    predictor = c("required_age", "platform_count", "required_age", "platform_count"),
    value = c(0, 3, 18, 1)
  )
  p <- build_numeric_distribution_plot(toy_long)

  root <- tempfile("numeric_outputs_")
  dir.create(root, recursive = TRUE)

  out <- save_numeric_distribution_outputs(
    numeric_grid_distribution = p,
    output_to_location_04 = root,
    figure_storage_path = root
  )

  expect_true(file.exists(out$rds_path))
  expect_true(file.exists(out$png_path))
})

test_that("save_numeric_distribution_outputs creates missing output directories", {
  toy_long <- data.frame(
    is_free = factor(c("Free", "Free", "Paid", "Paid")),
    predictor = c("required_age", "platform_count", "required_age", "platform_count"),
    value = c(0, 3, 18, 1)
  )
  p <- build_numeric_distribution_plot(toy_long)

  root <- tempfile("numeric_outputs_missing_")
  rds_dir <- file.path(root, "rds")
  fig_dir <- file.path(root, "fig")

  out <- save_numeric_distribution_outputs(
    numeric_grid_distribution = p,
    output_to_location_04 = rds_dir,
    figure_storage_path = fig_dir
  )

  expect_true(file.exists(out$rds_path))
  expect_true(file.exists(out$png_path))
})

test_that("save_numeric_distribution_outputs fails with wrong plot input", {
  root <- tempfile("numeric_outputs_bad_")
  dir.create(root, recursive = TRUE)

  expect_error(
    save_numeric_distribution_outputs(
      numeric_grid_distribution = data.frame(x = 1),
      output_to_location_04 = root,
      figure_storage_path = root
    ),
    "must be a ggplot object"
  )
})

test_that("run_numeric_features_distributions runs end-to-end and returns objects", {
  df_model <- make_toy_numeric_df()

  root <- tempfile("numeric_workflow_")
  input_dir <- file.path(root, "input")
  out_dir <- file.path(root, "output")
  fig_dir <- file.path(root, "fig")
  dir.create(input_dir, recursive = TRUE)
  saveRDS(df_model, file.path(input_dir, "wrangled_table.RDS"))

  out <- run_numeric_features_distributions(input_dir, out_dir, fig_dir)

  expect_true(is.data.frame(out$numeric_long))
  expect_s3_class(out$numeric_grid_distribution, "ggplot")
  expect_true(file.exists(out$saved_paths$rds_path))
  expect_true(file.exists(out$saved_paths$png_path))
})

test_that("run_numeric_features_distributions fails when wrangled_table.RDS is missing", {
  root <- tempfile("numeric_workflow_missing_")
  input_dir <- file.path(root, "input")
  dir.create(input_dir, recursive = TRUE)

  expect_error(
    run_numeric_features_distributions(input_dir, file.path(root, "output"), file.path(root, "fig")),
    "Input file not found"
  )
})
