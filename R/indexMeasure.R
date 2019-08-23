#' function that input a data table, returns a table that contains
#' mean_cross_validation with alpha-lambda value that makes the index maxium
#' methods represents specific methods you choose
#' @param data_table data table that
#' @param methods method that we care about
#' @param index index that we want to see the performance
#' @return NULL
#' @importFrom stats aggregate
#' @export
#'
#' @examples \dontrun{
#'  phenoSplit(data_table = a[[1]],
#'             methods = "CpGs_most_variablity",
#'             index = "Sensitivity")
#' }

indexMeasure <- function(data_table,
                         methods=c('CpGs_within_cluster','CpGs_most_variablity','CpGs_most_significant'),
                         index=c('Balanced.Accuracy','Sensitivity','Specificity','auc','F1')){

  test <- aggregate( . ~ alphaValue + lambaValue, data=data_table, FUN = "mean", na.action = NULL)
  test$NumOfPred <- round(test$NumOfPred,digits=0)
  test <- test[order(-test[,index]),]
  aa <- test[1,]
  aa <- aa[c("NumOfRep","alphaValue","lambaValue","NumOfPred",index)]
  aa$methods <- methods
  aa
}
