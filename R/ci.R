#' Confidence Interval
#'
#' Generate empirical confidence interval from a vector of iterations, e.g.
#' bootstrap or MCMC.
#'
#' @param x a vector of iterations.
#' @param level the significance level.
#'
#' @return Confidence bounds, a named vector of two elements.
#'
#' @note See \code{BCboot} for bootstrap bias correction.
#'
#' @importFrom stats quantile
#'
#' @export

ci <- function(x, level=0.95)
{
  alpha <- 1 - level
  lower <- alpha/2
  upper <- 1 - alpha/2

  quantile(x, probs=c(lower,upper))
}
