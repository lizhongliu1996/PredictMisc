#' wrapper function of three prediction methods
#'
#' @param predictMethod what prediction method to use
#' @param alphaValue vector that storage alpha values
#' @param PhenoTrain_df data frame that each row is a train samples
#' @param PhenoTest_df data frame that each row is a test samples
#' @param respCol_index response variable col number in beta data frame
#' @param outcome_type type of outcome variable, gauusian or binomial or poisson, etc
#' @param seed_int seed.value in use right row
#' @param whichRep_int repetition num in use right now, this decides on the traning data to be used
#' @param whichCVfold_int crossvalidation num in use right now, this decides on the traning data to be used
#' @param ncores number of cores to do parallel computing
#'
#' @return a list contains train data frame and test data frame and number of predictors
#' @export
#'
#' @examples \dontrun{
#'  data(ExampleMvalue_train)
#'  data(ExampleMvalue_test)
#'
#'  predict_ls <- predict_wrapper(
#'    predictMethod = "glmnet",
#'    alphaValue = seq(0, 1, by = 0.1),
#'    PhenoTrain_df = ExampleMvalue_train,
#'    PhenoTest_df = ExampleMvalue_test,
#'    respCol_index = 1,
#'    outcome_type = "binomial",
#'    seed_int = 123,
#'    whichRep_int = 1,
#'    whichCVfold_int = 1,
#'    ncores = 2
#'    )
#'  }
predict_wrapper <- function(predictMethod = c("glmnet","randomForest","svm"),
                            alphaValue, PhenoTrain_df, PhenoTest_df,
                            respCol_index, outcome_type, seed_int, whichRep_int,
                            whichCVfold_int, ncores){

  switch(
    predictMethod,
    "glmnet" = {
      glmnetFit_ls <- lapply(
        alphaValue,
        FUN = glmnetWrapper,
        trainTable_df = PhenoTrain_df,
        respCol_index = respCol_index,
        family = outcome_type,
        seed_int = seed_int,
        whichRep_int = whichRep_int,
        whichCVfold_int = whichCVfold_int
      )
      names(glmnetFit_ls) <- paste0("alpha", seq_len(length(alphaValue)))
      # glmnetFit_ls contains few slots, each slot is a glm results with different
      #   alpha value

      performance_ls <- mapply(
        FUN = evaluateGlmnet,
        fit = glmnetFit_ls,
        alpha = alphaValue,
        MoreArgs = list(testTable = PhenoTest_df,
                        respCol_index = respCol_index,
                        whichRep_int = whichRep_int,
                        whichCVfold_int = whichCVfold_int),
        SIMPLIFY = FALSE
      )

      Fit <- glmnetFit_ls
      performance_df <- do.call(rbind, performance_ls)
    },
    "randomForest" = {
      randomForest_ls <- caretWrapper(
        trainTable_df = PhenoTrain_df,
        respCol_index = respCol_index,
        predMethod = "rf",
        seed_int = seed_int,
        whichCVfold_int = whichCVfold_int,
        ncores = ncores
      )

      performance_df <- evaluateRF(
        caret_fit = randomForest_ls,
        testTable = PhenoTest_df,
        respCol_index = respCol_index,
        whichRep_int = whichRep_int,
        whichCVfold_int = whichCVfold_int
      )

      Fit <- randomForest_ls
    },
    "svm" = {
      svm_ls <- caretWrapper(
        trainTable_df = PhenoTrain_df,
        respCol_index = respCol_index,
        predMethod = "svm",
        seed_int = seed_int,
        whichCVfold_int = whichCVfold_int,
        ncores = ncores
      )

      performance_df <- evaluateSVM(
        caret_fit = svm_ls,
        testTable = PhenoTest_df,
        respCol_index = respCol_index,
        whichRep_int = whichRep_int,
        whichCVfold_int = whichCVfold_int
      )

      Fit <- svm_ls
    }
  )# end of switch

  list(
    Fit = Fit,
    performance = performance_df
  )


}
