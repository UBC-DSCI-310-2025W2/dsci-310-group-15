library(testthat)

make_validation_raw <- function(n = 6L) {
  raw <- data.frame(
    appid = seq_len(n),
    name_from_applist = paste("Game", seq_len(n)),
    app_details.data.required_age = rep(c("0", "18"), length.out = n),
    app_details.data.is_free = rep(c(TRUE, FALSE), length.out = n),
    app_details.data.type = rep(c("game", "demo"), length.out = n),
    app_details.data.release_date.date = rep("Jan 1, 2020", n),
    app_details.data.platforms.windows = rep(TRUE, n),
    app_details.data.platforms.mac = rep(c(TRUE, FALSE), length.out = n),
    app_details.data.platforms.linux = rep(FALSE, n)
  )

  raw[["app_details.data.categories"]] <- rep(list(list(list(description = "Single-player"))), n)
  raw[["app_details.data.dlc"]] <- rep(list(list()), n)
  raw[["app_details.data.demos"]] <- rep(list(list()), n)
  raw[["app_details.data.developers"]] <- rep(list(list("Dev")), n)
  raw[["app_details.data.publishers"]] <- rep(list(list("Pub")), n)
  raw
}

make_validation_model <- function(n = 10L) {
  data.frame(
    game_id = seq_len(n),
    game_name = paste("Game", seq_len(n)),
    is_free = factor(rep(c("Free", "Paid"), length.out = n), levels = c("Free", "Paid")),
    required_age = rep(c(0L, 18L), length.out = n),
    release_year = rep(c(2020, 2021), length.out = n),
    game_type = factor(rep(c("game", "demo"), length.out = n)),
    windows_support = rep(TRUE, n),
    mac_support = rep(c(TRUE, FALSE), length.out = n),
    linux_support = rep(c(FALSE, TRUE), length.out = n),
    platform_count = rep(2L, n),
    has_dlc = rep(c(TRUE, FALSE), length.out = n),
    has_demo = rep(c(FALSE, TRUE), length.out = n),
    n_categories = rep(c(1L, 2L), length.out = n),
    developer_name = paste("Dev", seq_len(n)),
    publisher_name = paste("Pub", seq_len(n)),
    cat_single_player = rep(c(TRUE, FALSE), length.out = n),
    cat_multi_player = rep(c(FALSE, TRUE), length.out = n)
  )
}

validation_status <- function(report, check) {
  report$status[match(check, report$check)]
}

test_that("load_validated_rds_data_frame validates RDS data frame files", {
  data_path <- tempfile(fileext = ".RDS")
  saveRDS(data.frame(x = 1), data_path)

  result <- load_validated_rds_data_frame(data_path, data_name = "file_check")
  expect_s3_class(result, "data.frame")
})

test_that("load_validated_rds_data_frame fails for non-data-frame RDS files", {
  data_path <- tempfile(fileext = ".RDS")
  saveRDS(c(1, 2, 3), data_path)

  expect_error(
    load_validated_rds_data_frame(data_path, data_name = "file_check"),
    "must be a data frame stored in an \\.RDS file"
  )
})

test_that("validate_raw_games_data passes a valid raw Steam fixture", {
  expect_silent(validate_raw_games_data(make_validation_raw()))
})

test_that("validate_raw_games_data catches missing columns and excessive missingness", {
  missing_column_raw <- make_validation_raw()
  missing_column_raw[["app_details.data.type"]] <- NULL

  expect_error(validate_raw_games_data(missing_column_raw))
})

test_that("validate_raw_games_data errors when required raw fields exceed missingness threshold", {
  high_missing_raw <- make_validation_raw()
  high_missing_raw[["app_details.data.is_free"]][] <- NA

  expect_error(
    validate_raw_games_data(high_missing_raw, missing_threshold = 0.15)
  )
})

test_that("validate_modeling_table passes a valid modeling fixture", {
  expect_silent(
    validate_modeling_table(
      make_validation_model(),
      expected_category_count = 2L
    )
  )
})

test_that("validate_modeling_table catches wrong types, duplicates, missing predictors, empty rows, bad levels, and ranges", {
  wrong_type <- make_validation_model()
  wrong_type$required_age <- as.character(wrong_type$required_age)

  duplicate_id <- make_validation_model()
  duplicate_id$game_id[2] <- duplicate_id$game_id[1]

  missing_predictor <- make_validation_model()
  missing_predictor$has_demo[1] <- NA

  empty_model <- make_validation_model()[0, ]

  bad_levels <- make_validation_model()
  bad_levels$is_free <- factor(
    c("Unknown", as.character(bad_levels$is_free[-1])),
    levels = c("Free", "Paid", "Unknown")
  )

  bad_range <- make_validation_model()
  bad_range$release_year[1] <- -2L

  expect_error(
    validate_modeling_table(wrong_type, expected_category_count = 2L),
    "required_age"
  )

  expect_error(
    validate_modeling_table(duplicate_id, expected_category_count = 2L),
  )

  expect_error(
    validate_modeling_table(missing_predictor, expected_category_count = 2L)
  )

  expect_error(
    validate_modeling_table(empty_model, expected_category_count = 2L)
  )

  expect_error(
    validate_modeling_table(bad_levels, expected_category_count = 2L),
  )

  expect_error(
    validate_modeling_table(bad_range, expected_category_count = 2L),
  )
})

test_that("optional metadata missingness warns but does not fail below the hard threshold", {
  model <- make_validation_model()
  model$publisher_name[1] <- NA_character_

  expect_warning(
    validate_modeling_table(
      model,
      expected_category_count = 2L,
      optional_metadata_fail_threshold = 0.25
    ),
    "publisher_name"
  )
})

test_that("validate_training_correlations fails target leakage predictors", {
  train_data <- make_validation_model()
  train_data$target_copy <- train_data$is_free == "Free"

  expect_error(
    suppressWarnings(
      validate_training_correlations(
        train_data,
        predictors = c("required_age", "target_copy")
      )
    ),
    "specially"
  )
})

test_that("validate_training_correlations warns and fails for high feature-feature correlations", {
  x <- seq_len(100)
  noisy_x <- x + rep(c(-20, 15, -10, 12, -5, 8, 0, -12, 10, -8), 10)
  train_data <- data.frame(
    is_free = factor(rep(c("Free", "Paid"), 50), levels = c("Free", "Paid")),
    x = x,
    noisy_x = noisy_x,
    duplicate_x = x
  )

  expect_warning(
    validate_training_correlations(
      train_data,
      predictors = c("x", "noisy_x"),
      feature_warn_threshold = 0.90,
      feature_fail_threshold = 0.999
    ))
  
  expect_error(
    suppressWarnings(
      validate_training_correlations(
        train_data,
        predictors = c("x", "duplicate_x"),
        feature_warn_threshold = 0.90,
        feature_fail_threshold = 0.98
      )
    ),
    "specially"
  )  
})
