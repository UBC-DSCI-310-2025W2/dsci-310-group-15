library(testthat)

test_that("summarize_release_counts_by_class excludes invalid release years", {
  release_counts <- summarize_release_counts_by_class(make_toy_target_summary_df())

  expect_true(is.data.frame(release_counts))
  expect_true(all(release_counts$release_year > 0))
  expect_true(all(c("release_year", "is_free", "n") %in% names(release_counts)))
})

test_that("summarize_binary_feature_rates returns one row per class and feature", {
  binary_rates <- summarize_binary_feature_rates(make_toy_target_summary_df())

  expect_true(is.data.frame(binary_rates))
  expect_equal(sort(unique(binary_rates$feature)), c("Has Demo", "Has DLC"))
  expect_equal(nrow(binary_rates), 4)
})

test_that("build_target_summary_plot returns a plot object", {
  target_summary_plot <- build_target_summary_plot(make_toy_target_summary_df())

  expect_s3_class(target_summary_plot, "ggplot")
})

test_that("run_target_summary_plots saves RDS and PNG outputs", {
  output_root <- tempfile("target_summary_")
  input_dir <- file.path(output_root, "input")
  object_dir <- file.path(output_root, "objects")
  figure_dir <- file.path(output_root, "figures")
  dir.create(input_dir, recursive = TRUE)
  saveRDS(make_toy_target_summary_df(), file.path(input_dir, "wrangled_table.RDS"))

  result <- run_target_summary_plots(
    input_data_dir = input_dir,
    output_object_dir = object_dir,
    output_figure_dir = figure_dir
  )

  expect_s3_class(result$target_summary_plot, "ggplot")
  expect_true(file.exists(result$saved_paths$rds_path))
  expect_true(file.exists(result$saved_paths$png_path))
})
