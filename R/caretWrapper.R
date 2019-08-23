#' warpper function of parameters for ramdom forest or suppport vector machine
#'
#' @param trainTable_df train data frame that each column represents a CpG probe
#'       while each row represents a sample, each cell is a methaylation M value
#' @param respCol_index response variable col number in beta data frame
#' @param predMethod use random forest or svm to do prediction
#' @param seed_int seed.value in use right row
#' @param whichCVfold_int crossvalidation num in use right now, this decides on the traning data to be used
#' @param ncores number of cores to do parallel computing
#'
#' @return A list contains results of glmnet
#' @importFrom caret train trainControl
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach registerDoSEQ
#'
#' @export
#'
#' @examples \dontrun{
#'  data(ExampleMvalue_train)
#'  fit <- caretWrapper(trainTable_df = ExampleMvalue_train,
#'                respCol_index = 1,
#'                predMethod = "rf",
#'                seed_int = 120,
#'                whichCVfold_int = 5,
#'                ncores = 2)
#'  }

caretWrapper <- function(trainTable_df,
                         respCol_index,
                         predMethod = c("rf","svmLinear"),
                         seed_int,
                         whichCVfold_int,
                         ncores = 2){

  seed.value.prediction <- seed_int + whichCVfold_int
  set.seed (seed.value.prediction)

  predMethod <- match.arg(predMethod)

  # if(predMethod == "rf"){
  #   method = "rf"
  # }else if(predMethod == "svm"){
  #   method = "svmLinear"
  # }

  cluster <- makeCluster(ncores) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  # The trainControl method controls resampling, which we do not need because
  #   we have explicitely split the test and training data.
  fitControl <- trainControl(
    method = "none",
    classProbs = TRUE,
    allowParallel = TRUE
  )

  fit <- train(
    x = as.matrix(trainTable_df[, -respCol_index]),
    y = trainTable_df[, respCol_index],
    method = predMethod,
    trControl = fitControl
  )

  stopCluster(cluster)
  # If the user wants to register their own cluster later, we need to delete
  #   ours first. See
  # https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  registerDoSEQ()

  fit
}
