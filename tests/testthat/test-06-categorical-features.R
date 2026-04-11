library(testthat)

test_that("compute_category_prevalence_gap returns sorted category gaps", {
  category_gap <- compute_category_prevalence_gap(make_toy_category_df())

  expect_true(is.data.frame(category_gap))
  expect_true(all(c("category", "Free", "Paid", "gap", "direction", "category_label") %in% names(category_gap)))
  expect_equal(category_gap$category[[1]], "cat_single_player")
})

test_that("compute_category_prevalence_gap errors when no category columns exist", {
  bad_df <- data.frame(
    game_id = 1:2,
    is_free = factor(c("Free", "Paid"), levels = c("Free", "Paid"))
  )

  expect_error(
    compute_category_prevalence_gap(bad_df),
    "No category columns found"
  )
})

test_that("build_category_gap_plot returns a ggplot object", {
  category_gap <- compute_category_prevalence_gap(make_toy_category_df())
  category_gap_plot <- build_category_gap_plot(category_gap)

  expect_s3_class(category_gap_plot, "ggplot")
})

test_that("run_categorical_features_plot saves RDS and PNG outputs", {
  output_root <- tempfile("categorical_features_")
  input_dir <- file.path(output_root, "input")
  object_dir <- file.path(output_root, "objects")
  figure_dir <- file.path(output_root, "figures")
  dir.create(input_dir, recursive = TRUE)
  saveRDS(make_toy_category_df(), file.path(input_dir, "wrangled_table.RDS"))

  result <- run_categorical_features_plot(
    input_data_dir = input_dir,
    output_object_dir = object_dir,
    output_figure_dir = figure_dir
  )

  expect_true(is.data.frame(result$category_gap))
  expect_s3_class(result$category_gap_plot, "ggplot")
  expect_true(file.exists(result$saved_paths$rds_path))
  expect_true(file.exists(result$saved_paths$png_path))
})
