#' Ricker Recruitment
#'
#' Calculate Ricker recruitment (Beverton-Holt formulation).
#'
#' @param S spawning biomass.
#' @param a initial slope.
#' @param b \code{1/Smax}.
#'
#' @return Recruitment as a number or vector.
#'
#' @note
#' Formulation from Beverton and Holt (1957, Eq. 6.16, p. 56).
#'
#' Scale the y axis with \code{a}.
#'
#' @export

ricker <- function(S, a, b)
{
  a*S*exp(-b*S)
}
