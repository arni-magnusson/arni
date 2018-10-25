#' Beverton-Holt Recruitment
#'
#' Calculate Beverton-Holt recruitment (Francis formulation).
#'
#' @param R0 average virgin recruitment.
#' @param h  steepness.
#' @param M  natural mortality rate.
#' @param mat  maturity at age.
#' @param w  weight at age.
#' @param B  spawning biomass.
#'
#' @return
#' Recruitment as a number or vector.
#'
#' @note
#' Formulation from Francis (1992, p. 929), where steepness is relative
#' recruitment at \code{0.2*B0}.
#'
#' @seealso
#' \code{\link{bevholt}}, \code{\link{bevholt.Rmax}}.
#'
#' @export

bevholt.h <- function(R0, h, M, mat, w, B)
{
  A <- length(w)
  a <- 1:(A-1)
  B0 <- sum(R0 * exp(-(a-1)*M) * mat[-A] * w[-A])  +
      R0 * exp(-(A-1)*M) * mat[A] * w[A] / (1-exp(-M))

  R <- 4*h*R0*B/B0 / (1-h+(5*h-1)*B/B0)

  R
}
