#' function that run parallel acluster algorithm given a methylation data frame
#'
#' @param chromeAnnot_df data frame contains 1 chrome annot information for 450k probes
#' @param tBeta_df a long format BetaValue_df to generate aclusters
#' @param minCpGs minimum number of cpgs within each cluster
#' @param ncores number of cores to do parallel computing
#' @return A list of elements containing cg probe names
#'
#' @import Aclust
#' @importFrom data.table as.data.table
#' @importFrom parallel makeCluster clusterEvalQ clusterExport  parLapplyLB stopCluster
#' @export
#'
#' @examples \dontrun{
#'  data(Example_df)
#'  data(chrome_annot_files)
#'  BetaTrans_df <- TransposeAssay(
#'  Example_df[, -1],
#'  omeNames = "rowNames"
#'  )
#'  runAcluster(
#'     chromeAnnot_df = chrome_annot_files,
#'     tBeta_df = BetaTrans_df,
#'     minCpGs = 5,
#'     ncores = 2
#'  )
#'}
#'

runAcluster <- function(chromeAnnot_df, tBeta_df, minCpGs = 5, ncores = 2){

  threads <- ncores
  cl <- makeCluster(threads)
  clusterEvalQ(cl, library(Aclust))
  clusterEvalQ(cl, library(data.table))
  clusterExport(cl, varlist = c("chromeAnnot_df", "tBeta_df"), envir = environment())

  aclust.list <- parLapplyLB(cl, chromeAnnot_df, function(item){
    out_ls <- assign.to.clusters(
      betas = tBeta_df,
      annot = as.data.table(item),
      dist.thresh = 0.5,
      bp.merge = 200,
      dist.type = "spearman",
      method = "complete"
    )
    gc()
    out_ls
    }
  )
  stopCluster(cl)

  aa <- unlist(aclust.list,recursive = FALSE)
  aa[lengths(aa) > 5]

}
