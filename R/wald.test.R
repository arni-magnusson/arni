#' Wald Test
#'
#' Perform Wald test.
#'
#' @param est a point estimate.
#' @param se the standard error of the point estimate.
#' @param mu the null hypothesis true mean.
#' @param conf.level a confidence level that will be used to evaluate a
#'        confidence interval.
#'
#' @note
#' Related to t-test and z-test, commonly used in nonlinear maximum likelihood
#' estimation.
#'
#' Assumes that the estimation error is normal-distributed and the standard
#' error is known.
#'
#' @return
#' List containing \code{z}, \code{p}, and \code{confint}.
#'
#' @export

wald.test <- function(est, se, mu, conf.level=0.95)
{
  z <- (est-mu) / se
  p <- 2 * pnorm(abs(z),lower.tail=FALSE)
  alpha <- 1 - conf.level
  confint <- est + qnorm(c(alpha/2, 1-alpha/2)) * se
  attr(confint,"conf.level") <- conf.level

  list(z=z, p=p, confint=confint)
}
