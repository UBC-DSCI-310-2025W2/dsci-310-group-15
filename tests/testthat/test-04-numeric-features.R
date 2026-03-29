library(testthat)

# ---- pivot_numeric_predictors: normal use ----

test_that("pivot_numeric_predictors returns long-format rows for default numeric predictors", {
  numeric_long <- pivot_numeric_predictors(make_toy_numeric_df())

  expect_true(is.data.frame(numeric_long))
  expect_true(all(c("target_class", "predictor", "value") %in% names(numeric_long)))
  expect_true(all(unique(numeric_long$predictor) %in% c("required_age", "release_year", "platform_count", "n_categories")))
})

test_that("pivot_numeric_predictors supports custom predictor subsets", {
  numeric_long <- pivot_numeric_predictors(
    data = make_toy_numeric_df(),
    predictors = c("required_age", "platform_count")
  )

  expect_equal(sort(unique(numeric_long$predictor)), c("platform_count", "required_age"))
})

# ---- pivot_numeric_predictors: edge case ----

test_that("pivot_numeric_predictors removes non-positive values only for configured predictors", {
  numeric_long <- pivot_numeric_predictors(make_toy_numeric_df(), drop_non_positive_for = "release_year")
  bad_release_year <- numeric_long$predictor == "release_year" & numeric_long$value <= 0

  expect_false(any(bad_release_year))
})

# ---- pivot_numeric_predictors: wrong input ----

test_that("pivot_numeric_predictors errors when a requested predictor is not numeric", {
  bad_df <- make_toy_numeric_df()
  bad_df$required_age <- as.character(bad_df$required_age)

  expect_error(
    pivot_numeric_predictors(bad_df, predictors = c("required_age", "platform_count")),
    "All predictors must be numeric"
  )
})

# ---- build_numeric_distribution_plot: normal use ----

test_that("build_numeric_distribution_plot returns a ggplot object for valid long data", {
  plot_obj <- build_numeric_distribution_plot(make_toy_numeric_long())
  expect_s3_class(plot_obj, "ggplot")
})

test_that("build_numeric_distribution_plot supports custom bin counts", {
  plot_obj <- build_numeric_distribution_plot(make_toy_numeric_long(), bins = 15)
  expect_s3_class(plot_obj, "ggplot")
})

# ---- build_numeric_distribution_plot: edge case ----

test_that("build_numeric_distribution_plot handles repeated numeric values", {
  repeated_long <- data.frame(
    target_class = factor(rep(c("Free", "Paid"), each = 4)),
    predictor = rep(c("required_age", "platform_count"), times = 4),
    value = rep(1, 8)
  )

  plot_obj <- build_numeric_distribution_plot(repeated_long)
  expect_s3_class(plot_obj, "ggplot")
})

# ---- build_numeric_distribution_plot: wrong input ----

test_that("build_numeric_distribution_plot errors when required columns are missing", {
  bad_long <- data.frame(target_class = factor(c("Free", "Paid")), predictor = c("a", "a"))

  expect_error(
    build_numeric_distribution_plot(bad_long),
    "missing required columns"
  )
})

# ---- save_numeric_distribution_outputs: normal use ----

test_that("save_numeric_distribution_outputs writes RDS and PNG files to existing directories", {
  plot_obj <- build_numeric_distribution_plot(make_toy_numeric_long())
  output_root <- tempfile("numeric_outputs_")
  dir.create(output_root, recursive = TRUE)

  saved <- save_numeric_distribution_outputs(
    numeric_grid_distribution = plot_obj,
    output_object_dir = output_root,
    output_figure_dir = output_root
  )

  expect_true(file.exists(saved$rds_path))
  expect_true(file.exists(saved$png_path))
})

# ---- save_numeric_distribution_outputs: edge case ----

test_that("save_numeric_distribution_outputs creates output directories when they do not exist", {
  plot_obj <- build_numeric_distribution_plot(make_toy_numeric_long())
  output_root <- tempfile("numeric_outputs_missing_")
  object_dir <- file.path(output_root, "rds")
  figure_dir <- file.path(output_root, "fig")

  saved <- save_numeric_distribution_outputs(
    numeric_grid_distribution = plot_obj,
    output_object_dir = object_dir,
    output_figure_dir = figure_dir
  )

  expect_true(file.exists(saved$rds_path))
  expect_true(file.exists(saved$png_path))
})

# ---- save_numeric_distribution_outputs: wrong input ----

test_that("save_numeric_distribution_outputs rejects non-ggplot input (data.frame is invalid)", {
  output_root <- tempfile("numeric_outputs_bad_")
  dir.create(output_root, recursive = TRUE)

  expect_error(
    save_numeric_distribution_outputs(
      numeric_grid_distribution = data.frame(x = 1),
      output_object_dir = output_root,
      output_figure_dir = output_root
    ),
    "must be a ggplot object"
  )
})

# ---- run_numeric_features_distributions: normal use ----

test_that("run_numeric_features_distributions returns pivoted data, plot, and saved paths end-to-end", {
  output_root <- tempfile("numeric_workflow_")
  input_dir <- file.path(output_root, "input")
  object_dir <- file.path(output_root, "output")
  figure_dir <- file.path(output_root, "fig")
  dir.create(input_dir, recursive = TRUE)
  saveRDS(make_toy_numeric_df(), file.path(input_dir, "wrangled_table.RDS"))

  result <- run_numeric_features_distributions(
    input_data_dir = input_dir,
    output_object_dir = object_dir,
    output_figure_dir = figure_dir
  )

  expect_true(is.data.frame(result$numeric_long))
  expect_s3_class(result$numeric_grid_distribution, "ggplot")
  expect_true(file.exists(result$saved_paths$rds_path))
  expect_true(file.exists(result$saved_paths$png_path))
})

# ---- run_numeric_features_distributions: wrong input ----

test_that("run_numeric_features_distributions errors when input RDS file is missing", {
  output_root <- tempfile("numeric_workflow_missing_")
  input_dir <- file.path(output_root, "input")
  dir.create(input_dir, recursive = TRUE)

  expect_error(
    run_numeric_features_distributions(
      input_data_dir = input_dir,
      output_object_dir = file.path(output_root, "output"),
      output_figure_dir = file.path(output_root, "fig")
    ),
    "Input file not found"
  )
})
