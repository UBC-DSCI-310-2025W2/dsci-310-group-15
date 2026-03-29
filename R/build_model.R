#' Creates a fitted and customized model using the caret library.
#' 
#' Quickly creates a pre-trained **classification** model. Starts with a training
#' control, with preset parameters for usability, then produces a model formula
#' given by the target and predictors, and finally produces a fitted model for use.
#' 
#' @param target A string containing the name of the target column.
#' @param predictors A vector of strings that contains the names of the predictor columns to be used.
#' @param train_split A data frame of the training data.
#' @param selected_model A string of the chosen classification model to work with.
#' @param selected_metric A string of the selected evaluation metric to use. Defaults to accuracy.
#' 
#' @return The fit model. Ready for use on a testing set.

library(caret)

build_model <- function(target, predictors, train_split, selected_model, selected_metric = "accuracy") {
  
  if (selected_model %in% c("ANFIS", "brnn", "bridge", "blassoAveraged",
                            "cubist", "DENFIS", "enet", "FIR.DM",
                            "GFS.FR.MOGUL", "GFS.THRIFT", "GFS.LT.RS",
                            "HYFIS", "icr", "lars", "lars2", "lm", 
                            "leapBackward", "leapForward", "leapSeq",
                            "lmStepAIC", "M5Rules", "M5", "glm.nb",
                            "neuralnet", "rqnc", "nnls", "penalized",
                            "krlsPoly", "pcr", "ppr", "qrf", "qrnn",
                            "rqlasso", "krlsRadial", "relaxo",
                            "rvmLinear", "rvmPoly", "rvmRadial",
                            "ridge", "foba", "rlm", "FS.HGD",
                            "spikeslab", "SBC", "superpc",
                            "blasso", "lasso", "WM")) {
    stop("Select a valid CLASSIFICATION model.")
  } else {
    ctrl <- caret::trainControl(
      method = "cv",
      number = 5, #Constant to ensure the function works on as many machines as possible.
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
    
    if (is.character(target) && is.character(selected_model) && is.character(selected_metric)) {
      model_formula <- stats::as.formula(
        paste(paste(target, '~'), paste(predictors, collapse = " + "))
      )
      
      fit_model <- caret::train(
        model_formula,
        data = train_split,
        method = selected_model,
        family = binomial(),
        metric = selected_metric,
        trControl = ctrl
      ) } else {
        stop("One or more of your inputs is not a string. Please change it into a string.")
      }
  }
  fit_model
}