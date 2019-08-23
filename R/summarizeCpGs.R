#' summarize function that choose cpgs based on different methods
#'
#' @param clust_ls list of cpgs, each slot contains few cpgs
#' @param train_df data frame that each row is a train samples
#' @param test_df data frame that each row is a test samples
#' @param selectMethod what cpg selection method to use, use full cpgs \link{fullCpGs}
#' within cluster or PC1 score \link{getPC1} of cluster or maximum \link{maxCpGs}
#' expression score, default set to fullcpgs, it is feasible to write new methods
#' by adding a new function that take in train/test dataset and cpglist then
#' return a list of train and test subset data.
#'
#' @return a list contains train data frame and test data frame and number of predictors
#' @export
#'
#' @examples \dontrun{
#' data(aclust.listDemo)
#' data(ExampleMvalue_train)
#' data(ExampleMvalue_test)
#'
#' test <- summarizeCpGs(
#'   clust_ls = aclust.listDemo,
#'   train_df = ExampleMvalue_train[ , -1],
#'   test_df = ExampleMvalue_test[ , -1],
#'   selectMethod = fullCpGs
#'  )
#' }
summarizeCpGs <- function(clust_ls, train_df, test_df, selectMethod){
  selectMethod(clust_ls, train_df, test_df)
}

