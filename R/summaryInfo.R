#' function that create a information data frame that storage all parameters we need
#'
#' @param NumOfRep number of repeat measurements in this trial
#' @param NumOfCv number of cross validation in this trail
#' @param seed seed value
#' @param phenoData_df a data frame contains sample id(in first column) and variables
#' we interested in(in the second column), the first column should named "Sample"
#' @param respCol_index response variable col number in beta data frame
#' @return A data frame containing design experiment information
#'   \itemize{
#'     \item{\code{NumOfRep} : }{number of repeat measurements in this trial
#'       }
#'     \item{\code{NumOfCv} : }{number of cross validation in this trail
#'       }
#'     \item{\code{seed} : }{seed value for trail
#'       }
#'     \item{\code{...} : }{sample names
#'       }
#'   }
#'
#' @importFrom caret createFolds
#'
#' @export
#'
#' @examples \dontrun{
#'  data(pheno)
#'  information_df <- summaryInfo(
#'   NumOfRep = 10,
#'   NumOfCv = 5,
#'   seed = 122,
#'   phenoData_df = pheno,
#'   respCol_index = 2
#'  )
#' }
summaryInfo <- function(NumOfRep, NumOfCv, seed, phenoData_df, respCol_index){

   ##create df for storage
   information_df <- expand.grid(NumOfCv = 1:NumOfCv, NumOfRep = 1:NumOfRep)
   information_df <- information_df[ ,c("NumOfRep","NumOfCv")]
   information_df$seed <- seed + information_df$NumOfRep + information_df$NumOfCv

   ### divide based on different rep
   dd <- split(information_df, information_df$NumOfRep)

   phenoData_df <- as.data.frame(phenoData_df, stringsAsFactors = FALSE)

    xx <- function(df){
      ### for each rep. create folds to divide patients into train and test
            set.seed(df$seed[1])
            flds <-  createFolds(phenoData_df[,respCol_index],
                           k = NumOfCv,
                           list = TRUE,
                           returnTrain = F)

            ###for each cv, choose different train and test
            xx <- lapply(1:nrow(df),
                         FUN = phenoSplit,
                         flds = flds,
                         pheno = phenoData_df,
                         info = df)

            do.call(rbind,xx)
    }

    qq <- lapply(dd, FUN = xx)
    qq <- do.call(rbind, qq)
    rownames(qq) <- NULL

    cbind(information_df, qq)
}
