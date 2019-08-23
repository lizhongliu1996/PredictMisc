#' Evaluate \code{glmnet} Prediction on Methylation Data
#'
#' @description Read data, based on one row of information_df, then selected
#'   most significant cpgs as predictors to fit elastic net, random forest or
#'   support vector machine model and evaluate its prediction performance.
#'
#' @param rowNum num of row in information_df
#' @param Beta_df Beta_df is a data frame that each row is a cpg probe, each col
#'   is a sample id, each cell is a Beta value, first column is the phenodata, please
#'   make sure this column is a factor with levels, or we can not ensure accuracy
#'   of the results
#' @param beta2M whether transfre beta to m value before prediction
#' @param respCol_index response variable col number in beta data frame
#' @param designInfo_df information df generate by \link{summaryInfo} function
#' @param alphaValue vector that storage alpha values
#' @param ncores number of cores to do parallel computing
#' @param npredictors number of cpgs chosen to be predictors(
#' default set to 5k, suggest 5k, 10k, 20k, 50k)
#' @param predictMethod what prediction method to use
#' @param outcome_type type of outcome variable, gauusian or binomial or poisson, etc
#' @param resultPath path to storage results
#' @param save whether to save the results
#'
#' @return return a list with three elements,\cr
#' \enumerate{
#'     \item first element is the fit model results of different prediction methods
#'     \item Second item second element is the data frame that contains evalutation parameters of
#'           different prediction methods' performace:\cr
#'         for glmnet net, the data frame has row number equal to number of alpha
#'           values given in the function argument times 16 columns with different
#'           evaluation parameters including NumOfRep,NumOfCv, auc_results,
#'           Sensitivity, Specificity, etc;\cr
#'         for random forest and support vector machine, the data frame has one
#'           row times 14 columns with different evaluation parameters including
#'           NumOfRep,NumOfCv, auc_results, Sensitivity, Specificity, etc\cr
#'     \item third element is a vector that indicate number of predictors used
#'  }
#' @details
#' \describe{
#'  \item{predictMethod1: }{Elastic net from function \link[glmnet]{glmnet}
#'      to do prediction}
#'  \item{predictMethod2: }{Random Forest from function \link[caret]{train}
#'      to do prediction(requires package "randomForest" installed first)}
#'  \item{predictMethod3: }{Support Vector Machine from function
#'      \link[caret]{train} to do prediction(requires package "kernlab" installed)}
#' }
#' @importFrom pathwayPCA TransposeAssay
#'
#' @export
#'
#' @examples \dontrun{
#'  data(Example_df)
#'  data(pfcInfo_df)
#'
#'  test <- pipeMostSig(
#'    rowNum = 10,
#'    Beta_df = Example_df,
#'    beta2M = TRUE,
#'    respCol_index = 1,
#'    designInfo_df = pfcInfo_df,
#'    alphaValue = seq(0, 1, by = 0.1),
#'    ncores = 2,
#'    npredictors = 5000,
#'    predictMethod = "glmnet",
#'    outcome_type = "binomial",
#'    save = FALSE,
#'    resultPath = NULL
#'  )
#' }

pipeMostSig <- function(rowNum, Beta_df, beta2M = TRUE, respCol_index, designInfo_df,
                        alphaValue = seq(0, 1, by = 0.1),
                        ncores = 2, npredictors = 5000,
                        predictMethod = c("glmnet", "randomForest", "svm"),
                        outcome_type = "binomial",
                        save = FALSE, resultPath = NULL){

  ## 1. divided methylation data into train and test data  ####################
  TrainTest <- methSplit(
    rowNum = rowNum,
    Beta_df = Beta_df,
    designInfo_df = designInfo_df
  )
  BetaPhenoTrain_df <- TrainTest$Train_df
  BetaPhenoTest_df <- TrainTest$Test_df

  #whether transfer to mvalue
  trans_list <- beta2M_wrapper(
    beta2M = beta2M,
    BetaPhenoTrain_df = BetaPhenoTrain_df,
    BetaPhenoTest_df = BetaPhenoTest_df,
    respCol_index = respCol_index,
    returnType = "matrix"
  )

  Train_mat <- trans_list$Train
  Test_mat <- trans_list$Test

  ## compute p-value
  ## first divided pheno into two groups, do a two sample t-test
  ## select cpgs that has p-value < 0.05 as predictors
  diffGroup <- split(
    x = BetaPhenoTrain_df,
    f = BetaPhenoTrain_df[, respCol_index]
  )
  group1Name <-  rownames(diffGroup[[1]])
  group2Name <-  rownames(diffGroup[[2]])

  group1_mat <- Train_mat[group1Name, ]
  group2_mat <- Train_mat[group2Name, ]

  ### do parallel t test
  pvalue_vec <- parTtest(group1_mat, group2_mat, ncores = ncores)
  p.sorted_vec <- sort(pvalue_vec, decreasing = TRUE)

  ## 2. choose predictors
  predictors <- names(p.sorted_vec)[seq_len(npredictors)]
  p <- length(predictors)

  ##  subset based on predictors then transfer beta value into mvalue ###########
  Train_df <- as.data.frame(Train_mat[ , predictors])
  Test_df  <- as.data.frame(Test_mat[ , predictors])

  PhenoTrain_df <- cbind(
    BetaPhenoTrain_df[ , respCol_index],
    Train_df
  )
  PhenoTest_df <- cbind(
    BetaPhenoTest_df[ , respCol_index],
    Test_df
  )

  ## 4. fit model and do prediction  ##########################################
  seed.value <- designInfo_df$seed[rowNum]
  NumOfRep   <- designInfo_df$NumOfRep[rowNum]
  NumOfCv    <- designInfo_df$NumOfCv[rowNum]

  predict_ls <- predict_wrapper(
    predictMethod = predictMethod,
    alphaValue = alphaValue,
    PhenoTrain_df = PhenoTrain_df,
    PhenoTest_df = PhenoTest_df,
    respCol_index = respCol_index,
    outcome_type = outcome_type,
    seed_int = seed.value,
    whichRep_int = NumOfRep,
    whichCVfold_int = NumOfCv,
    ncores = ncores
  )

  out_ls <- list(
    Fit = predict_ls$Fit,
    PredPerformance = predict_ls$performance,
    numOfPredictors = p
  )

  if(save){
    saveRDS(out_ls, paste0(resultPath,"sig-", npredictors,"-",
                           predictMethod,"_row", rowNum,".RDS"))
  }

  out_ls

}
