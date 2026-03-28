library(testthat)

test_that("summarize_class_distribution works for standard Free/Paid data", {
  toy_df <- data.frame(
    is_free = factor(c("Free", "Paid", "Free"), levels = c("Free", "Paid"))
  )

  out <- summarize_class_distribution(toy_df)

  expect_true(is.data.frame(out))
  expect_true(all(c("is_free", "n", "pct", "label") %in% names(out)))
  expect_equal(sum(out$n), 3)
  expect_equal(round(sum(out$pct), 6), 1)
})

test_that("summarize_class_distribution supports a custom target column", {
  toy_df <- data.frame(
    target = factor(c("Yes", "No", "Yes"))
  )

  out <- summarize_class_distribution(toy_df, target_col = "target")

  expect_true(is.data.frame(out))
  expect_true("is_free" %in% names(out))
  expect_equal(sum(out$n), 3)
})

test_that("summarize_class_distribution handles single-class edge case", {
  toy_df <- data.frame(
    is_free = factor(rep("Free", 4), levels = c("Free", "Paid"))
  )

  out <- summarize_class_distribution(toy_df)

  expect_equal(nrow(out), 1)
  expect_equal(out$pct[[1]], 1)
})

test_that("summarize_class_distribution fails with missing target column", {
  toy_df <- data.frame(other = c(1, 2, 3))

  expect_error(
    summarize_class_distribution(toy_df),
    "missing required columns"
  )
})

test_that("build_class_distribution_plot returns a ggplot object", {
  class_counts <- data.frame(
    is_free = factor(c("Free", "Paid"), levels = c("Free", "Paid")),
    n = c(10, 5),
    pct = c(2 / 3, 1 / 3),
    label = c("10 (66.7%)", "5 (33.3%)")
  )

  p <- build_class_distribution_plot(class_counts)
  expect_s3_class(p, "ggplot")
})

test_that("build_class_distribution_plot supports one-class edge case", {
  class_counts <- data.frame(
    is_free = factor("Free", levels = c("Free", "Paid")),
    n = 10,
    pct = 1,
    label = "10 (100%)"
  )

  p <- build_class_distribution_plot(class_counts)
  expect_s3_class(p, "ggplot")
})

test_that("build_class_distribution_plot fails when required columns are missing", {
  bad_counts <- data.frame(
    is_free = factor(c("Free", "Paid")),
    n = c(1, 1)
  )

  expect_error(
    build_class_distribution_plot(bad_counts),
    "missing required columns"
  )
})

test_that("save_class_distribution_outputs saves files in existing directories", {
  class_counts <- data.frame(
    is_free = factor(c("Free", "Paid"), levels = c("Free", "Paid")),
    n = c(10, 5),
    pct = c(2 / 3, 1 / 3),
    label = c("10 (66.7%)", "5 (33.3%)")
  )
  p <- build_class_distribution_plot(class_counts)

  root <- tempfile("class_outputs_")
  dir.create(root, recursive = TRUE)

  out <- save_class_distribution_outputs(
    class_distribution_plot = p,
    output_to_location_03 = root,
    figure_storage_path = root
  )

  expect_true(file.exists(out$rds_path))
  expect_true(file.exists(out$png_path))
})

test_that("save_class_distribution_outputs creates missing output directories", {
  class_counts <- data.frame(
    is_free = factor(c("Free", "Paid"), levels = c("Free", "Paid")),
    n = c(10, 5),
    pct = c(2 / 3, 1 / 3),
    label = c("10 (66.7%)", "5 (33.3%)")
  )
  p <- build_class_distribution_plot(class_counts)

  root <- tempfile("class_outputs_missing_")
  rds_dir <- file.path(root, "rds")
  fig_dir <- file.path(root, "fig")

  out <- save_class_distribution_outputs(
    class_distribution_plot = p,
    output_to_location_03 = rds_dir,
    figure_storage_path = fig_dir
  )

  expect_true(file.exists(out$rds_path))
  expect_true(file.exists(out$png_path))
})

test_that("save_class_distribution_outputs fails with wrong plot input", {
  root <- tempfile("class_outputs_bad_")
  dir.create(root, recursive = TRUE)

  expect_error(
    save_class_distribution_outputs(
      class_distribution_plot = data.frame(x = 1),
      output_to_location_03 = root,
      figure_storage_path = root
    ),
    "must be a ggplot object"
  )
})

test_that("run_class_imbalance_check runs end-to-end and returns objects", {
  df_model <- data.frame(
    is_free = factor(c("Free", "Paid", "Free"), levels = c("Free", "Paid"))
  )

  root <- tempfile("class_workflow_")
  input_dir <- file.path(root, "input")
  out_dir <- file.path(root, "output")
  fig_dir <- file.path(root, "fig")
  dir.create(input_dir, recursive = TRUE)
  saveRDS(df_model, file.path(input_dir, "wrangled_table.RDS"))

  out <- run_class_imbalance_check(input_dir, out_dir, fig_dir)

  expect_true(is.data.frame(out$class_counts))
  expect_s3_class(out$class_distribution_plot, "ggplot")
  expect_true(file.exists(out$saved_paths$rds_path))
  expect_true(file.exists(out$saved_paths$png_path))
})

test_that("run_class_imbalance_check fails when wrangled_table.RDS is missing", {
  root <- tempfile("class_workflow_missing_")
  input_dir <- file.path(root, "input")
  dir.create(input_dir, recursive = TRUE)

  expect_error(
    run_class_imbalance_check(input_dir, file.path(root, "output"), file.path(root, "fig")),
    "Input file not found"
  )
})
