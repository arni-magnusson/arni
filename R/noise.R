#' Gaussian Noise
#'
#' Generate Gaussian noise with autocorrelation.
#'
#' @param n length.
#' @param mu mean.
#' @param sigma standard deviation (\code{0} to \code{Inf}).
#' @param rho autocorrelation (\code{-1} to \code{1}).
#'
#' @note
#' Based on ADCAM.
#'
#' @return
#' Numeric vector whose mean, standard deviation, and autocorrelation is close
#' to \code{mu}, \code{sigma}, and \code{rho}.
#'
#' @seealso
#' \code{\link{rho}}.
#'
#' @importFrom stats rnorm
#'
#' @export

noise <- function(n, mu=0, sigma=1, rho=0)
{
  x <- numeric(n)
  eps <- rnorm(n, sd=sigma)
  sqrt1minusrho2.eps <- sqrt(1-rho^2) * eps

  if(n > 1)
  {
    x[1] <- sqrt1minusrho2.eps[1]
    for(i in seq_len(n-1))
      x[i+1] <- rho*x[i] + sqrt1minusrho2.eps[i+1]
    x <- x + mu
  }

  x
}
