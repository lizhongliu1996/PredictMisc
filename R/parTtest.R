#' function that do parallel t test and elect results based on p.threshold
#'
#' @param group1 df that each col is a probe id, each row is a sample
#' @param group2 df that each col is a probe id, each row is a sample
#' @param ncores number of cores to do parallel computing
#'
#' @return NULL
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#' @importFrom parallel parSapply
#' @importFrom stats t.test
#'
#' @export
#'
#' @examples \dontrun{
#'  parTtest(earlygroup, lategroup, ncores = 2)
#' }

parTtest <- function(group1, group2, ncores = 2){

  threads <- ncores
  cl <- makeCluster(threads)
  clusterExport(cl, varlist = c("group1", "group2"), envir = environment())

  lk <- parSapply(cl,seq_len(ncol(group1)), function(col){
    t.test(group1[, col],group2[, col])$p.value
  })

  names(lk) <- colnames(group1)
  stopCluster(cl)
  lk

}


