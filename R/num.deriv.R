#' Numerical Derivatives
#'
#' Calculate numerical derivatives of a univariate function.
#'
#' @param f a function.
#' @param x a vector of values to pass to \code{f}.
#' @param delta the width of each \code{x} window, in which the slope at
#'        \code{x} will be evaluated.
#' @param align alignment of \code{x} window (\code{-1}: left, \code{0}: center,
#'        \code{1}: right).
#' @param \dots passed to \code{f}.
#'
#' @return Vector of numerical derivatives.
#'
#' @note Simple finite-difference approximation: \eqn{\Delta f / \Delta x}.
#'
#' @export

num.deriv <- function(f, x, delta=1e-4, align=0, ...)
{
  align <- match.arg(as.character(align), c("-1","0","1"))

  if(align == "-1")
  {
    x1 <- x - delta
    x2 <- x
  }
  else if(align == "0")
  {
    x1 <- x - delta/2
    x2 <- x + delta/2
  }
  else if(align == "1")
  {
    x1 <- x
    x2 <- x + delta
  }

  slope <- (f(x2,...) - f(x1,...)) / delta

  slope
}
