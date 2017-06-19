#' Univariate Optimization
#'
#' Univariate optimization.
#'
#' @param f a function to minimize, by varying the first argument.
#' @param interval a vector containing the lower and upper limits of the
#'        interval to search over.
#' @param tol the max difference between the optimal \code{x} and returned
#'        \code{x}.
#' @param hessian whether to return the Hessian matrix, standard error, and 95\%
#'        confidence interval.
#' @param \dots passed to the \code{f} function.
#'
#' @return
#' List containing \code{x} (optimal value), \code{f} (function value), and
#' \code{n} (number of function evaluations).
#'
#' @note
#' The number of function evaluations (\code{n}) is two times the number of
#' loops performed.
#'
#' @importFrom stats optimHess qnorm
#'
#' @export

bisection <- function(f, interval, tol=1e-8, hessian=FALSE, ...)
{
  lower <- interval[1]
  upper <- interval[2]

  n <- 0
  x <- (lower+upper) / 2

  while(upper-x > tol)
  {
    if(f(x,...) > f(x+tol,...))  # current slope is negative, discard left side
      lower <- x
    else                         # current slope is positive, discard right side
      upper <- x
    n <- n + 2                   # two function evals per loop
    x <- (lower+upper) / 2
  }

  output <- list(x=x, f=f(x,...), n=n)

  if(hessian)
  {
    h <- optimHess(fn=f, x, ...)
    se <- sqrt(diag(solve(h)))
    ci95 <- x + qnorm(c(0.025,0.5,0.975)) * se
    output <- c(output, list(h=h, se=se, ci95=ci95))
  }

  output
}
