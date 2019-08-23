#' get PC1 of cpg list
#'
#' @description given a beta matrix with list of cpgs, extract pc1 of each slot
#'    of the list and return the pc1 score data frame
#'
#' @param clust_ls list of cpgs
#' @param train_df beta or M value matrix with train samples
#' @param test_df beta or M value matrix with test samples
#'
#' @return a list contains performance of predicition
#'
#' @importFrom pathwayPCA TransposeAssay CreatePathwayCollection CreateOmics
#' @importFrom pathwayPCA ExtractAESPCs LoadOntoPCs
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom parallel detectCores
#'
#' @export
#'
#' @examples \dontrun{
#'   data(aclust.listDemo)
#'   data(ExampleMvalue_train)
#'   data(ExampleMvalue_test)
#'
#'    test <- getPC1(
#'    clust_ls = aclust.listDemo,
#'    train_df = ExampleMvalue_train[,-1],
#'    test_df = ExampleMvalue_test[,-1]
#'  )
#' }
getPC1 <- function(clust_ls, train_df, test_df){

  n <- length(clust_ls)

  terms <- paste0("cluster", seq_len(n))

  aclustpath <- CreatePathwayCollection(
    clust_ls,
    terms,
    setType = "pathways"
  )

  train_df2 <- rownames_to_column(train_df, "Sample")
  test_df2 <- rownames_to_column(test_df, "Sample")

  methOmics <- CreateOmics(
    train_df2,
    aclustpath,
    response = NULL,
    respType = "none"
  )

  methPCs <- ExtractAESPCs(
    methOmics, numPCs = 1,
    parallel = TRUE,
    numCores = 2,
    standardPCA = TRUE
  )

  ff <- unlist(methPCs$PCs,recursive = FALSE)
  MTrain_df <- as.data.frame(t(do.call(rbind,ff)))
  rownames(MTrain_df) <- rownames(train_df)
  colnames(MTrain_df) <- paste0("path", seq_len(n))

  MTest_df <- LoadOntoPCs(
    design_df = test_df2,
    loadings_ls = methPCs$loadings
  )
  MTest_df <- column_to_rownames(MTest_df, "SampleID")

  list(
    train_df = MTrain_df,
    test_df = MTest_df,
    npredictors = n
  )

}
