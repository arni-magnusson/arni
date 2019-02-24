#' Von Bertalanffy Growth
#'
#' Evaluate the von Bertalanffy growth function.
#'
#' @param ages a vector of ages for which lengths should be predicted.
#' @param Linf von Bertalanffy growth parameter.
#' @param K von Bertalanffy growth parameter.
#' @param t0 von Bertalanffy growth parameter.
#'
#' @return Vector of predicted lengths
#'
#' @export

vonBert <- function(ages=1:10, Linf=100, K=0.2, t0=0)
{
  Linf * (1 - exp(-K*(ages-t0)))
}
