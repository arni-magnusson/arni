#' Ricker Recruitment
#'
#' Calculate Ricker recruitment (Bjornsson-Magnusson formulation).
#'
#' @param Rmax maximum recruitment.
#' @param Smax stock size that gives \code{Rmax}.
#' @param S is spawning biomass.
#'
#' @return Recruitment as a number or vector.
#'
#' @note
#' Formulation from Bjornsson and Magnusson (2009, Eq. 2.5, p. 6).
#'
#' Scale the y axis with \code{Rmax}.
#'
#' @export

ricker.Rmax <- function(Rmax, Smax, S)
{
  Rmax * S/Smax * exp(1-S/Smax)
}
