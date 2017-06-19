#' Beverton-Holt Recruitment
#'
#' Calculate Beverton-Holt recruitment.
#'
#' @param S spawning biomass.
#' @param a initial slope.
#' @param b carrying capacity.
#'
#' @return
#' Recruitment as a number or vector.
#'
#' @note
#' Formulation from Hilborn and Walters (1992, Eq. 7.5.3, p. 258).
#'
#' Scale the y axis by multiplying \code{a} and \code{b} by same amount.
#'
#' @export

bevholt <- function(S, a, b)
{
  a*S / (1+a*S/b)
}
