library(testthat)

test_that("create_stratified_train_test_split returns complementary partitions", {
  toy_model <- make_toy_model_df()

  split <- create_stratified_train_test_split(
    toy_model,
    target_col = "is_free",
    train_prop = 0.8,
    seed = 123
  )

  expect_equal(length(intersect(split$train_indices, split$test_indices)), 0)
  expect_equal(sort(c(split$train_indices, split$test_indices)), seq_len(nrow(toy_model)))
  expect_equal(nrow(split$train_data) + nrow(split$test_data), nrow(toy_model))
})

test_that("select_model_predictors includes base predictors and category columns", {
  predictors <- select_model_predictors(make_toy_model_df())

  expect_true(all(c("required_age", "release_year", "cat_single_player") %in% predictors))
})

test_that("create_stratified_train_test_split rejects invalid split proportions", {
  expect_error(
    create_stratified_train_test_split(make_toy_model_df(), train_prop = 1),
    "between 0 and 1"
  )
})

test_that("create_train_test_split in train_test_split_data.R returns complementary partitions", {
  split <- create_train_test_split(normal1_df, normal1_df$target, 0.8, seed = 123)

  expect_equal(length(intersect(split$train_index, split$test_index)), 0)
  expect_equal(sort(c(split$train_index, split$test_index)), seq_len(nrow(normal1_df)))
})
