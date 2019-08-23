#' function that set parameters for glmnet
#'
#' @param nAlpha how many number of alpha values you want
#' @param nLambda how many number of nLambda values you want
#' @return A list of two elements containing the alpha and lambda value
#'   \itemize{
#'     \item{\code{alpha} : }{alpha value for glmnet
#'       }
#'     \item{\code{lambda} : }{lambda value for glmnet
#'       }
#'   }
#' @export
#'
#' @examples \dontrun{
#'  parameters_ls <- setParameters(nAlpha = 11,
#'                                nLambda = 11)
#' }

setParameters <- function(nAlpha, nLambda){

  alpha <- seq(0, 1.0, length = nAlpha)
  test <- seq(-5, 5, length = nLambda)
  lambda <- 10^test

  expand.grid(alpha = alpha, lambda = lambda)

}
