library(testthat)

# ---- summarize_class_distribution: normal use ----

test_that("summarize_class_distribution returns count, proportion, and labels for a binary target", {
  class_summary <- summarize_class_distribution(make_toy_class_df())

  expect_true(is.data.frame(class_summary))
  expect_true(all(c("target_class", "n", "pct", "label") %in% names(class_summary)))
  expect_equal(sum(class_summary$n), 3)
  expect_equal(round(sum(class_summary$pct), 6), 1)
})

test_that("summarize_class_distribution supports custom target column names", {
  toy_df <- data.frame(outcome = factor(c("yes", "no", "yes")))
  class_summary <- summarize_class_distribution(toy_df, target_col = "outcome")

  expect_true("target_class" %in% names(class_summary))
  expect_equal(sum(class_summary$n), 3)
})

# ---- summarize_class_distribution: edge case ----

test_that("summarize_class_distribution handles a single observed class", {
  one_class_df <- data.frame(is_free = factor(rep("Free", 4), levels = c("Free", "Paid")))
  class_summary <- summarize_class_distribution(one_class_df)

  expect_equal(nrow(class_summary), 1)
  expect_equal(class_summary$pct[[1]], 1)
})

# ---- summarize_class_distribution: wrong input ----

test_that("summarize_class_distribution errors when target column is missing", {
  expect_error(
    summarize_class_distribution(data.frame(other = c(1, 2, 3))),
    "missing required columns"
  )
})

# ---- build_class_distribution_plot: normal use ----

test_that("build_class_distribution_plot returns a ggplot object for valid class summary input", {
  plot_obj <- build_class_distribution_plot(make_toy_class_counts())
  expect_s3_class(plot_obj, "ggplot")
})

test_that("build_class_distribution_plot accepts a custom color palette", {
  custom_palette <- c("Free" = "#1D4ED8", "Paid" = "#0EA5E9")
  plot_obj <- build_class_distribution_plot(make_toy_class_counts(), fill_palette = custom_palette)

  expect_s3_class(plot_obj, "ggplot")
})

# ---- build_class_distribution_plot: edge case ----

test_that("build_class_distribution_plot handles one-class summaries", {
  one_class_counts <- data.frame(
    target_class = factor("Free", levels = c("Free", "Paid")),
    n = 10,
    pct = 1,
    label = "10 (100%)"
  )

  plot_obj <- build_class_distribution_plot(one_class_counts)
  expect_s3_class(plot_obj, "ggplot")
})

# ---- build_class_distribution_plot: wrong input ----

test_that("build_class_distribution_plot errors when required columns are missing", {
  bad_counts <- data.frame(target_class = factor(c("Free", "Paid")), n = c(1, 1))

  expect_error(
    build_class_distribution_plot(bad_counts),
    "missing required columns"
  )
})

# ---- save_class_distribution_outputs: normal use ----

test_that("save_class_distribution_outputs writes RDS and PNG files to existing directories", {
  plot_obj <- build_class_distribution_plot(make_toy_class_counts())
  output_root <- tempfile("class_outputs_")
  dir.create(output_root, recursive = TRUE)

  saved <- save_class_distribution_outputs(
    class_distribution_plot = plot_obj,
    output_object_dir = output_root,
    output_figure_dir = output_root
  )

  expect_true(file.exists(saved$rds_path))
  expect_true(file.exists(saved$png_path))
})

# ---- save_class_distribution_outputs: edge case ----

test_that("save_class_distribution_outputs creates output directories when they do not exist", {
  plot_obj <- build_class_distribution_plot(make_toy_class_counts())
  output_root <- tempfile("class_outputs_missing_")
  object_dir <- file.path(output_root, "rds")
  figure_dir <- file.path(output_root, "fig")

  saved <- save_class_distribution_outputs(
    class_distribution_plot = plot_obj,
    output_object_dir = object_dir,
    output_figure_dir = figure_dir
  )

  expect_true(file.exists(saved$rds_path))
  expect_true(file.exists(saved$png_path))
})

# ---- save_class_distribution_outputs: wrong input ----

test_that("save_class_distribution_outputs rejects non-ggplot input (data.frame is invalid)", {
  output_root <- tempfile("class_outputs_bad_")
  dir.create(output_root, recursive = TRUE)

  expect_error(
    save_class_distribution_outputs(
      class_distribution_plot = data.frame(x = 1),
      output_object_dir = output_root,
      output_figure_dir = output_root
    ),
    "must be a ggplot object"
  )
})

# ---- run_class_imbalance_check: normal use ----

test_that("run_class_imbalance_check returns summary, plot, and saved paths end-to-end", {
  output_root <- tempfile("class_workflow_")
  input_dir <- file.path(output_root, "input")
  object_dir <- file.path(output_root, "output")
  figure_dir <- file.path(output_root, "fig")
  dir.create(input_dir, recursive = TRUE)
  saveRDS(make_toy_class_df(), file.path(input_dir, "wrangled_table.RDS"))

  result <- run_class_imbalance_check(
    input_data_dir = input_dir,
    output_object_dir = object_dir,
    output_figure_dir = figure_dir
  )

  expect_true(is.data.frame(result$class_counts))
  expect_s3_class(result$class_distribution_plot, "ggplot")
  expect_true(file.exists(result$saved_paths$rds_path))
  expect_true(file.exists(result$saved_paths$png_path))
})

# ---- run_class_imbalance_check: wrong input ----

test_that("run_class_imbalance_check errors when input RDS file is missing", {
  output_root <- tempfile("class_workflow_missing_")
  input_dir <- file.path(output_root, "input")
  dir.create(input_dir, recursive = TRUE)

  expect_error(
    run_class_imbalance_check(
      input_data_dir = input_dir,
      output_object_dir = file.path(output_root, "output"),
      output_figure_dir = file.path(output_root, "fig")
    ),
    "Input file not found"
  )
})
