#' function to do prediction and evaluate based on the results of support vector machine
#'
#' @param caret_fit results of random forest or support vector machine
#' @param testTable test data frame that each column represents a CpG probe
#'    while each row represents a sample, each cell is a M value, first column
#'    is phenodata
#' @param respCol_index col number of response variable
#' @param whichRep_int repetition num in use right now, this decides on the
#' traning data to be used
#' @param whichCVfold_int crossvalidation num in use right now, this decides
#' on the traning data to be used
#'
#' @return A data frame of containing the train and test beta matrix
#'   \itemize{
#'     \item{\code{auc_results} : }{auc value of prediction}
#'     \item{\code{Sensitivity} : }{Sensitivity value of prediction}
#'     \item{\code{Specificity} : }{Specificity value of prediction}
#'     \item{\code{...} : }{more index}
#'   }
#'
#' @importFrom pROC roc auc
#' @importFrom caret confusionMatrix
#' @import kernlab
#'
#' @export
#'
#' @examples \dontrun{
#'    data(ExampleMvalue_test)
#'    data(svm_Fit)
#'
#'    test <- evaluateSVM(
#'      caret_fit = svm_Fit,
#'      testTable = ExampleMvalue_test,
#'      respCol_index = 1,
#'      whichRep_int = 1,
#'      whichCVfold_int = 1
#'    )
#'  }

evaluateSVM <- function(caret_fit, testTable, respCol_index, whichRep_int,
                        whichCVfold_int){

  fit <- caret_fit$finalModel

  testX = as.matrix(testTable[, -respCol_index])

  pred.fit <- kernlab::predict(
    fit,
    testX,
    type = "response"
  )

  pred.prob <- kernlab::predict(
    object = fit,
    newdata = testX,
    type = "prob"
  )

  ##match prob with fit outcome
  num <- as.numeric(pred.fit)
  pred.prob <- sapply(1:nrow(testX), function(i){
    pred.prob[i,num[i]]
  })

  pred.factor <- as.factor(pred.fit)

  confMatrix <- confusionMatrix(
    data = pred.factor,
    reference = as.factor(testTable[, respCol_index])
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
    auc_results,
    as.data.frame(t(confMatrix$byClass))
  )

}
