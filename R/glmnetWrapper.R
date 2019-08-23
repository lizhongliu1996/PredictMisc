#' warpper function of parameters for glmnet function
#'
#' @param alpha one alpha value for glmnet
#' @param trainTable_df train data frame that each column represents a CpG probe
#'       while each row represents a sample, each cell is a methaylation M value
#' @param respCol_index which col number is response variable in Methylation Mvalue data frame
#' @param family Response type
#' @param seed_int seed.value in use right row
#' @param whichRep_int repetition num in use right now, this decides on the traning data to be used
#' @param whichCVfold_int crossvalidation num in use right now, this decides on the traning data to be used
#'
#' @details function that create input of function \link{evaluateGlmnet}
#'
#' @return A list contains results of glmnet
#' @importFrom glmnet cv.glmnet
#'
#' @export
#'
#' @examples \dontrun{
#'  data(ExampleMvalue_train)
#'  fit <- glmnetWrapper(
#'     alpha = 0,
#'     trainTable_df = ExampleMvalue_train,
#'     respCol_index = 1,
#'     family = "binomial",
#'     seed_int = 120,
#'     whichRep_int = 1,
#'     whichCVfold_int = 5
#'    )
#'  }
glmnetWrapper <- function(alpha, trainTable_df, respCol_index,
                          family = c("gaussian", "binomial", "poisson",
                                     "multinomial", "cox", "mgaussian"),
                          seed_int, whichRep_int, whichCVfold_int){

  family <- match.arg(family)
  seed.value.prediction <- seed_int + whichCVfold_int + whichRep_int
  set.seed(seed.value.prediction)

  # calculate what fraction of the total each class has
  y = trainTable_df[, respCol_index]
  fraction <- table(y)/length(y)
  # assign 1 - that value to a "weights" vector
  weights <- 1 - fraction[as.character(y)]

  fold_int <- sample(
    seq_len(5),
    size = nrow(trainTable_df),
    replace = TRUE
  )

  cv.glmnet(
    x = as.matrix(trainTable_df[, -respCol_index]),
    y = trainTable_df[, respCol_index],
    weights = weights,
    foldid = fold_int,
    family = family,
    alpha = alpha
  )

}
