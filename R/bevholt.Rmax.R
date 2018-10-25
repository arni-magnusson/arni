#' Beverton-Holt Recruitment
#'
#' Calculate Beverton-Holt recruitment (Haddon formulation).
#'
#' @param Rmax maximum recruitment.
#' @param S50 stock size that gives 1/2 \code{Rmax}.
#' @param S spawning biomass.
#'
#' @return
#' Recruitment as a number or vector.
#'
#' @note
#' Formulation from Haddon (2001, Eq. 9.2, p. 252).
#'
#' Scale the y axis with \code{Rmax}.
#'
#' @seealso
#' \code{\link{bevholt}}, \code{\link{bevholt.h}}.
#'
#' @export

bevholt.Rmax <- function(Rmax, S50, S)
{
  Rmax * S/(S+S50)
}
