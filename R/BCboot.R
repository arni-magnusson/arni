#' Apply Bias Correction to Bootstrap Estimates
#'
#' Apply bias correction to bootstrap estimates.
#'
#' @param thetastar a vector of bootstrap estimates.
#' @param thetahat a point estimate from original data.
#' @param bounds a vector of lower and upper limits to handle extremely biased
#'        cases.
#'
#' @return
#' Vector of bias-corrected bootstrap estimates.
#'
#' @note
#' BCa with zero acceleration, based on \code{bcanon} in package \pkg{bootstrap}
#' by Tibshirani.
#'
#' See Efron and Tibshirani (1993, pp. 184-186), Gavaris and Van Eeckhaute
#' (1998, p.10), and Gavaris (1999, p. 47).
#'
#' @importFrom stats qnorm pnorm approx
#'
#' @export

BCboot <- function(thetastar, thetahat, bounds=c(0.1,0.9))
{
  B <- length(thetastar)
  alpha <- (1:B) / B
  lower <- bounds[1]
  upper <- bounds[2]

  z0 <- qnorm(max(lower, min(upper, sum(thetastar<thetahat)/B)))
  zalpha <- qnorm(alpha)
  newalpha <- pnorm(2*z0 + zalpha)
  Omegainv <- approx(alpha, sort(thetastar), newalpha, rule=2)$y
  bias.corrected <- Omegainv[rank(thetastar)]

  bias.corrected
}
