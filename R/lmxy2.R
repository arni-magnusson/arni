#' Multiple LM
#'
#' Fit multiple LM fast, only returning the coefficients.
#'
#' @param X a matrix containing predictor values and 1 in first column.
#' @param y a vector (or single-column matrix) containing response values.
#'
#' @return
#' Single-column matrix containing the coefficients.
#'
#' @seealso
#' \code{\link{lmxy1}}.
#'
#' @export

lmxy2 <- function(X, y)
{
  XtX <- crossprod(X,X)
  Xty <- crossprod(X,y)

  beta <- solve(XtX, Xty)

  beta
}
