#' SE from Hessian
#'
#' Evaluate SE or correlation matrix from Hessian matrix.
#'
#' @param H a Hessian matrix.
#' @param cor whether to return correlation matrix instead of SE.
#'
#' @note
#' Covariance matrix can be useful for applying the delta method.
#'
#' @return
#' Vector of standard errors, or if \code{cor = TRUE} a correlation matrix.
#'
#' @export

hess <- function(H, cor=FALSE)
{
  V <- solve(H)                      # covariance matrix
  sigma <- sqrt(diag(V))             # std errors
  if(cor)
    output <- V / (sigma %o% sigma)  # correlation matrix
  else
    output <- sigma

  output
}
