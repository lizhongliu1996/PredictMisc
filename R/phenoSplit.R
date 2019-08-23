#' function that split phenoData into train and test
#'
#' @param rowNum num of row in information_df
#' @param flds a list by create by first part of function summaryInfo
#' @param pheno phenotype data frame
#' @param info df create by first part of function summaryInfo
#' @return NULL
#' @importFrom pathwayPCA TransposeAssay
#' @importFrom tibble column_to_rownames
#'
#' @export
#'
#' @examples \dontrun{
#'  test <- phenoSplit(rowNum = 3,
#'                       flds = flds,
#'                      pheno = patients_df,
#'                       info = information_df)
#' }
phenoSplit <- function(rowNum, flds, pheno, info){

  pheno$ToT <- "Test"
  a <- info$NumOfCv[rowNum]
  pheno[-(flds[[a]]),]$ToT <- "Train"
  pheno <- column_to_rownames(pheno, "Sample")
  aa <- TransposeAssay(pheno["ToT"], omeNames = "rowNames", stringsAsFactors = FALSE)
  rownames(aa) <- NULL
  aa

}


