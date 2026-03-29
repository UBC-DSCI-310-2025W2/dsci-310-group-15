library(testthat)

source("../../R/test_data.R")

#Normal Use Case ----
test_that("Given a regular, valid data frame, the function will correctly split the data into its appropriate partitions.",
          {
            expect_equal(nrow(test_data(normal1_df, normal1_df$target, 0.75)), round((1 - 0.75) * nrow(normal1_df)), 1)
            expect_equal(nrow(test_data(normal1_df, normal1_df$target, 0.5)), (1 - 0.5) * nrow(normal1_df))
          })

test_that("Given just a target column, the function will still perform the split correctly.", {
  expect_equal(nrow(test_data(target_df, target_df$target, 0.75)), round((1 - 0.75) * nrow(target_df)), 1)
  expect_equal(nrow(test_data(normal1_df, normal1_df$target, 0.5)), (1 - 0.5) * nrow(normal1_df))
})

test_that("The function should return a data frame.", {
  expect_s3_class(test_data(normal1_df, normal1_df$target, 0.8), "tbl_df")
})

#Edge Use Case ----
test_that("Given an unreasonable split size (0 or 1), the function terminates and gives an error.", {
  expect_error(test_data(normal1_df, normal1_df$target, 1))
})

#Wrong Input Case ----
test_that("If the data frame is not actually a data frame, the function terminates and gives an error.", {
  expect_error(test_data(fake_df, fake_df$target, 0.75))
})

test_that("If the function does not receive `target_col` as a slice of a data frame, it will terminate and give an error.", {
  expect_error(test_data(normal1_df, normal1_df$wowee, 0.8))
})