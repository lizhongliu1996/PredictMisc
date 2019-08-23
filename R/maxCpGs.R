#' select method that find the maximum expression within a cluster
#'
#' @param clust_ls list of cpgs, each slot contains few cpgs
#' @param train_df data frame that each row is a train samples
#' @param test_df data frame that each row is a test samples
#'
#' @return a list contains train data frame and test data frame and number of predictors
#' @export
#'
#' @examples \dontrun{
#'  data(aclust.listDemo)
#'  data(ExampleMvalue_train)
#'  data(ExampleMvalue_test)
#'
#'  test <- maxCpGs(
#'    clust_ls = aclust.listDemo,
#'    train_df = ExampleMvalue_train[,-1],
#'    test_df = ExampleMvalue_test[,-1]
#'  )
#' }
maxCpGs <- function(clust_ls, train_df, test_df){

  predictors <- unlist(clust_ls)

  Train_df <- train_df[, predictors]
  Test_df  <- test_df[, predictors]

  maxTrain_vec <- apply(Train_df,1,max)
  maxTest_vec <- apply(Test_df,1,max)

  list(
    train_df = maxTrain_vec,
    test_df = maxTest_vec,
    npredictors = 1
  )
}
