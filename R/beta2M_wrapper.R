#' wrapper function to do beta to m value transformation
#' @param beta2M whether transfre beta to m value before prediction
#' @param BetaPhenoTrain_df train data frame that each column represents a CpG probe
#'    while each row represents a sample, each cell is a M value, first column
#'    is phenodata
#' @param BetaPhenoTest_df test data frame that each column represents a CpG probe
#'    while each row represents a sample, each cell is a M value, first column
#'    is phenodata
#' @param respCol_index response variable col number in beta data frame
#' @param returnType return data type, data frame or matrix, default set to matrix
#'
#' @return A list of two elements: train data frame and test data frame
#'
#' @export
#'
#' @examples \dontrun{
#'    data(ExampleMvalue_train)
#'    data(ExampleMvalue_test)
#'
#'    test <- beta2M_wrapper(
#'      beta2M = TRUE,
#'      BetaPhenoTrain_df = ExampleMvalue_train,
#'      BetaPhenoTest_df = ExampleMvalue_test,
#'      respCol_index = 1,
#'      returnType = "matrix"
#'    )
#'  }
beta2M_wrapper <- function(beta2M = TRUE, BetaPhenoTrain_df, BetaPhenoTest_df,
                           respCol_index, returnType = "matrix"){

  if(beta2M == TRUE){
    ##transfer to matrix to make transform much faster
    BetaPhenoTrain_mat <- as.matrix(BetaPhenoTrain_df[, -respCol_index])
    BetaPhenoTest_mat <- as.matrix(BetaPhenoTest_df[, -respCol_index])
    if(returnType == "matrix"){
      Train <- logitB(BetaPhenoTrain_mat, B = 2)
      Test <- logitB(BetaPhenoTest_mat, B = 2)
    }else if(returnType == "data.frame"){
      Train <- as.data.frame(logitB(BetaPhenoTrain_mat, B = 2))
      Test <- as.data.frame(logitB(BetaPhenoTest_mat, B = 2))
    }
  }else{
    if(returnType == "matrix"){
      Train <- as.matrix(BetaPhenoTrain_df[, -respCol_index])
      Test <- as.matrix(BetaPhenoTest_df[, -respCol_index])
    }else if(returnType == "data.frame"){
      Train <- BetaPhenoTrain_df[, -respCol_index]
      Test <- BetaPhenoTest_df[, -respCol_index]
    }
  }

  list(Train = Train,
       Test = Test)
}
