#' function that split methylation data into train and test
#'
#' @param rowNum number of row of information_df
#' @param Beta_df data frame that each row is a cpg probe, each col is a
#' sample id, each cell is a M value, first column is the phenodata
#' @param designInfo_df df create by function \link{summaryInfo}
#' @return A list of two elements containing the train and test beta matrix
#'   \itemize{
#'     \item{\code{Train_df} : }{A data frame of the train sample beta values
#'       }
#'     \item{\code{Test_df} : }{A data frame of the test sample beta values
#'       }
#'   }
#' @importFrom pathwayPCA TransposeAssay
#'
#' @export
#'
#' @examples \dontrun{
#'  data(Example_df)
#'  data(pfcInfo_df)
#'  data_ls <- methSplit(
#'    rowNum = 1,
#'    Beta_df = Example_df,
#'    designInfo_df = pfcInfo_df
#'  )
#' }

methSplit <- function(rowNum, Beta_df, designInfo_df){

  ### remove  column that contains NumOfRep NumOfcv and seed
  infoRow <- designInfo_df[rowNum, -(1:3)]
  infoCol <- TransposeAssay(infoRow, omeNames = "rowNames", stringsAsFactors = FALSE)

  #status of samples
  train_idx  <- infoCol[,1] == "Train"

  list (Train_df = Beta_df[train_idx, ],
        Test_df  = Beta_df[!train_idx, ])

}

