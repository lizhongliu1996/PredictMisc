#' function that do logit transformation
#'
#' @param x a numeric or complex vector.
#' @param B a positive or complex number: the base with respect to which
#' logarithms are computed. Defaults to e=exp(1)

#' @return value after logit transformation
#'
#' @export
#'
#' @examples \dontrun{
#'   logitB(0.5, B = 2)
#'
#' }

logitB <- function(x, B = exp(1)){
  log(x / (1 - x), base = B)
}
