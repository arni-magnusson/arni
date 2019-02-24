#' Fournier Likelihood
#'
#' Evaluate Fournier likelihood, Coleraine-style.
#'
#' @param P observed catch at age.
#' @param Phat predicted catch at age.
#' @param n sample size, one number or vector.
#'
#' @return Negative log likelihood.
#'
#' @export

fournier <- function(P, Phat, n)
{
  A <- ncol(P)

  xiplus = P*(1-P) + 0.1/A  # constant
  expbracket = exp(-(P-Phat)^2 / (2*xiplus*1/n))
  neglogL = sum(0.5*log(xiplus)) - sum(log(expbracket+0.01))

  neglogL
}
