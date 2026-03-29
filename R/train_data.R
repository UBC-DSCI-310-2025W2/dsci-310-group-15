#' Creates training portion of a data frame.
#' 
#' Given a data frame, knowing the target and predictor columns, splits the data into its training split.
#' Uses the caret function `createDataPartition` to perform the split.
#' Returns a data frame.
#' This function keeps all of the original columns, and can work on only a target column.
#' 
#' You must set your own seed to ensure the result can be reproduced.
#' 
#' @param df The data frame to be split.
#' @param target_col The target column of the data frame that will be split. (e.g. df$target)
#'    The `$` takes the target column from the data frame.
#' @param partition_size How large the TRAINING portion will be (must be a float greater than 0 and less than 1).
#' 
#' @return A data frame containing the test split of the data.
#'    It will contain all the columns. It allows you the freedom to pick and test which variables will contribute the most.
#'
#'@examples
#'\dontrun{train_data(iris, iris$class, 0.7) #Assuming you have the iris dataset loaded in.}

train_data <- function(df, target_col, partition_size) {
    if (partition_size > 0 && partition_size < 1) {
      
      train_idx <- caret::createDataPartition(target_col, p = partition_size, list = FALSE)
      train_df <- df[train_idx, ]
      
    } else {
      stop("You did not pick a valid option for the training split. Pick a float between 0 and 1.")
    }
  train_df
  }