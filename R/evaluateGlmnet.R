#' function to do prediction and evaluate based on the results of glmnet
#'
#'
#' @param fit fit result of glmnet
#' @param alpha specific alpha value use for this glmnet fit object
#' @param testTable test data frame that each column represents a CpG probe
#'    while each row represents a sample, each cell is a M value, first column
#'    is phenodata
#' @param respCol_index col number of response variable
#' @param whichRep_int number of repetation
#' @param whichCVfold_int number of cross validation
#'
#'
#' @details evaluate performance of results return by function \link{glmnetWrapper}
#'
#' @return A data frame contains parameters for evaluate prediction performance
#'   \itemize{
#'     \item{\code{NumOfRep} : }{Number of repetation}
#'     \item{\code{NumOfCv} : }{Number of cross validation}
#'     \item{\code{alphaValue} : }{alpha value of glmnet}
#'     \item{\code{lambda.min} : }{lambda min value of glmnet}
#'     \item{\code{auc_results} : }{auc value of prediction}
#'     \item{\code{Sensitivity} : }{Sensitivity value of prediction}
#'     \item{\code{Specificity} : }{Specificity value of prediction}
#'     \item{\code{...} : }{more index}
#'   }
#'
#'
#' @importFrom stats predict
#' @importFrom pROC roc auc
#' @importFrom caret confusionMatrix
#'
#'
#' @export
#'
#' @examples \dontrun{
#'    data(ExampleMvalue_test)
#'    data(glmnet_Fit_ls)
#'    alphaValue = seq(0, 1, by = 0.1)
#'
#'    test <- evaluateGlmnet(
#'      fit = glmnet_Fit_ls[[1]],
#'      alpha = alphaValue[1],
#'      testTable = ExampleMvalue_test,
#'      respCol_index = 1,
#'      whichRep_int = 1,
#'      whichCVfold_int = 1
#'      )
#'  }

evaluateGlmnet <-function(fit, alpha, testTable, respCol_index, whichRep_int,
                          whichCVfold_int){

  lambda.min <- fit$lambda.min

  pred.fit <- predict(
    object = fit,
    newx = as.matrix(testTable[, -respCol_index]),
    s = "lambda.min",
    type = "class"
  )

  pred.prob <- predict(
    object = fit,
    newx = as.matrix(testTable[, -respCol_index]),
    s = "lambda.min",
    type = "response"
  )

  # pred.factor <- as.factor(pred.fit)
  # pred.prob <- as.numeric(pred.prob)
  #
  # confMatrix <- confusionMatrix(
  #   data = pred.factor,
  #   reference = as.factor(testTable[, respCol_index])
  # )

  refer = levels(testTable[, respCol_index])
  pred.factor <- factor(pred.fit, levels = refer)
  pred.prob <- as.numeric(pred.prob)

  confMatrix <- confusionMatrix(
    data = pred.factor,
    reference = testTable[, respCol_index]
  )

  roc_results <- roc(
    response = testTable[, respCol_index],
    predictor = pred.prob,
    plot = FALSE
  )

  auc_results <- auc(roc_results)

  data.frame(
    NumOfRep = whichRep_int,
    NumOfCv = whichCVfold_int,
    alphaValue = alpha,
    lambdaMin = lambda.min,
    auc_results,
    as.data.frame(t(confMatrix$byClass))
  )

}
