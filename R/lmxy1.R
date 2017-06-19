#' Simple LM
#'
#' Fit a simple LM fast, only returning the coefficients.
#'
#' @param x a vector containing the predictor values.
#' @param y a vector containing the response values.
#'
#' @return
#' Vector containing the coefficients.
#'
#' @seealso
#' \code{\link{lmxy2}}.
#'
#' @export

lmxy1 <- function(x, y)
{
  xbar <- mean(x)
  ybar <- mean(y)
  x_xbar <- x - xbar
  y_ybar <- y - ybar

  b1 <- sum(x_xbar*y_ybar) / sum(x_xbar*x_xbar)
  b0 <- ybar - b1*xbar
  beta <- c(b0, b1)

  beta
}
