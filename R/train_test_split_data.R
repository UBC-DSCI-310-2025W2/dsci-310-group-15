#' Validate train/test split inputs
#'
#' @param df Data frame to split.
#' @param target_col Target vector used for stratification.
#' @param partition_size Proportion of rows assigned to the training split.
#'
#' @return `TRUE` invisibly when validation passes.
validate_train_test_split_inputs <- function(df, target_col, partition_size) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (length(target_col) != nrow(df)) {
    stop("`target_col` must be a vector with one value per row in `df`.", call. = FALSE)
  }

  if (!is.numeric(partition_size) || length(partition_size) != 1L ||
      is.na(partition_size) || partition_size <= 0 || partition_size >= 1) {
    stop("`partition_size` must be a single number between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Create complementary train and test splits
#'
#' Uses `caret::createDataPartition()` once, then derives both train and test rows
#' from the same index vector. Use this function when both splits are needed
#' together.
#'
#' @param df Data frame to split.
#' @param target_col Target vector used for stratification.
#' @param partition_size Proportion of rows assigned to the training split.
#' @param seed Optional random seed for reproducible splitting.
#'
#' @return A list containing `train`, `test`, `train_index`, and `test_index`.
#' @examples
#' \dontrun{
#' split <- create_train_test_split(iris, iris$Species, 0.8, seed = 123)
#' }
create_train_test_split <- function(df, target_col, partition_size, seed = NULL) {
  validate_train_test_split_inputs(df, target_col, partition_size)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("`seed` must be `NULL` or a single number.", call. = FALSE)
    }
    set.seed(seed)
  }

  train_index <- as.integer(caret::createDataPartition(
    target_col,
    p = partition_size,
    list = FALSE
  ))
  all_index <- seq_len(nrow(df))
  test_index <- setdiff(all_index, train_index)

  list(
    train = df[train_index, , drop = FALSE],
    test = df[test_index, , drop = FALSE],
    train_index = train_index,
    test_index = test_index
  )
}

#' Create the training portion of a data frame
#'
#' @param df Data frame to split.
#' @param target_col Target vector used for stratification.
#' @param partition_size Proportion of rows assigned to the training split.
#'
#' @return A data frame containing the training split.
#' @examples
#' \dontrun{
#' train_split_data(iris, iris$Species, 0.8)
#' }
train_split_data <- function(df, target_col, partition_size) {
  create_train_test_split(df, target_col, partition_size)$train
}

#' Create the testing portion of a data frame
#'
#' @param df Data frame to split.
#' @param target_col Target vector used for stratification.
#' @param partition_size Proportion of rows assigned to the training split.
#'
#' @return A data frame containing the testing split.
#' @examples
#' \dontrun{
#' test_split_data(iris, iris$Species, 0.8)
#' }
test_split_data <- function(df, target_col, partition_size) {
  create_train_test_split(df, target_col, partition_size)$test
}
