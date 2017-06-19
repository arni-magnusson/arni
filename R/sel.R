#' Selectivity
#'
#' Calculate selectivity at age for Coleraine double-normal curve.
#'
#' @param Sfull age at full selectivity.
#' @param Sleft a shape parameter for the left hand slope.
#' @param Sright a shape parameter for the right hand slope.
#' @param ages a vector of ages.
#'
#' @note
#' The shape parameter for each slope is log variance. Use \code{-15} for
#' knife-edge and \code{15} for flat selectivity.
#'
#' @references
#' Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J., and Starr,
#'  P. (2003) \emph{Coleraine: A generalized age-structured stock
#'   assessment model.} User's manual version 2.0. University of
#'  Washington Report SAFS-UW-0116. Available at
#'  \url{http://fish.washington.edu/research/coleraine/pdf/coleraine.pdf}.
#'
#' @return
#' Vector of selectivity values.
#'
#' @export

sel <- function(Sfull, Sleft, Sright=15, ages=1:10)
{
  Aleft <- ages[ages<=Sfull]
  Aright <- ages[ages>Sfull]
  S <- c(exp(-(Aleft-Sfull)^2/exp(Sleft)), exp(-(Aright-Sfull)^2/exp(Sright)))
  S
}
