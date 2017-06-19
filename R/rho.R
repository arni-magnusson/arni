#' Autocorrelation
#'
#' Calculate serial autocorrelation with lag 1.
#'
#' @param x a vector of values.
#'
#' @return
#' Autocorrelation as a number.
#'
#' @note
#' Gives same value as \code{acf(lag=1)}.
#'
#' In the calculations, the two vectors \code{x[t]} and \code{x[t+1]} are
#' compared to the same \code{xbar}.
#'
#' Therefore, the estimated autocorrelation is slightly lower than
#' \code{cor(x[t], x[t+1])} and also lower than \code{cov(x[t], x[t+1]) /
#' var(x)}, where \code{x[t]} and \code{x[t+1]} are not compared to a common
#' mean.
#'
#' This function serves as a shorthand for \code{acf(x, plot=FALSE,
#' lag.max=1)$acf[2]} and makes the underlying equation transparent.
#'
#' @seealso
#' \code{\link{noise}}.
#'
#' @export

rho <- function(x)
{
  t <- seq_len(length(x) - 1)
  xbar <- mean(x)
  rho <- sum((x[t]-xbar)*(x[t+1]-xbar)) / sum((x-xbar)^2)
  rho
}
