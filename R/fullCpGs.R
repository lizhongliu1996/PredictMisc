#' select method that use all cpgs within aclust or cometh selection cpgs.
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
#'  test <- fullCpGs(
#'    clust_ls = aclust.listDemo,
#'    train_df = ExampleMvalue_train[,-1],
#'    test_df = ExampleMvalue_test[,-1]
#'  )
#' }
fullCpGs <- function(clust_ls, train_df, test_df){

  predictors <- unlist(clust_ls)
  p <- length(predictors)

  Train_df <- train_df[, predictors]
  Test_df  <- test_df[, predictors]

  list(
    train_df = Train_df,
    test_df = Test_df,
    npredictors = p
  )
}
