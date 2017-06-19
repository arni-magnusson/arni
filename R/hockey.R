#' Hockey-Stick Recruitment
#'
#' Calculate hockey-stick recruitment.
#'
#' @param S spawning biomass.
#' @param Rmax maximum recruitment.
#' @param Smax lowest \code{S} that produces \code{Rmax}.
#' @param eta an optional smoothness parameter, e.g. \code{0.001}.
#'
#' @note
#' Smoothness parameter is based on Mesnil and Rochet (2010), but replacing
#' \verb{gamma^2/4} with \verb{eta*Smax^2}.
#'
#' @return
#' Recruitment as a number or vector.
#'
#' @export

hockey <- function(S, Smax, Rmax, eta=0)
{
  numerator <- S + Smax*sqrt(1+eta) - sqrt((Smax-S)^2+eta*Smax^2)
  denominator <- Smax + Smax*sqrt(1+eta)
  R <- Rmax * numerator / denominator

  R
}
