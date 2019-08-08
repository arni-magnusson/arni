#' Normal Likelihood
#'
#' Calculate the negative log likelihood for a normal distribution.
#'
#' @param obs vector of observed values.
#' @param fit vector of fitted values.
#'
#' @return
#' Negative log likelihood.
#'
#' @note
#' The negative log likelihood is calculated as:
#' \deqn{-\log L=0.5n\log(2\pi) + n\log\sigma + \frac{RSS}{2\sigma^2}}{-log
#'   L = 0.5n log(2 pi) + n log sigma + RSS/(2 sigma^2)}
#'
#' @export

norm.like <- function(obs, fit)
{
  n <- length(obs)
  res <- obs - fit
  sigma <- sdp(res)
  RSS <- sum(res^2)
  neglogL <- 0.5*n*log(2*pi) + n*log(sigma) + RSS/(2*sigma^2)
  neglogL
}
