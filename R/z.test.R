#' Z-Test.
#'
#' Perform z-test
#'
#' @param x a vector of data values, or the mean of data values.
#' @param mu the null hypothesis true mean.
#' @param sigma the standard deviation of data values in the population.
#' @param n the number of data values.
#' @param conf.level a confidence level that will be used to evaluate a
#'        confidence interval.
#'
#' @note
#' Similar to t-test, but assumes that sigma is known without error.
#'
#' It is wrong to assume that \code{sigma=sqrt(SS/n-1)} or \code{sqrt(SS/n)},
#' unless \code{x} is the complete population.
#'
#' It also makes no sense to perform a z-test if \code{x} is the complete
#' population, since the true mean is known.
#'
#' Rarely used in statistical inference, unlike the related \code{t.test} and
#' \code{wald.test} that are commonly used.
#'
#' @return
#' List containing \code{xbar}, \code{z}, \code{p}, and \code{confint}.
#'
#' @export

z.test <- function(x, mu, sigma, n=length(x), conf.level=0.95)
{
  xbar <- mean(x)
  se <- sigma/sqrt(n)

  z <- (xbar-mu) / se
  p <- 2 * pnorm(abs(z),lower.tail=FALSE)
  alpha <- 1 - conf.level
  confint <- xbar + qnorm(c(alpha/2, 1-alpha/2)) * se
  attr(confint,"conf.level") <- conf.level

  list(xbar=xbar, z=z, p=p, confint=confint)
}
