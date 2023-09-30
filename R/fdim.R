#' FLR Dimensions
#'
#' Show dimensions of an FLR object.
#'
#' @param x FLR object.
#'
#' @return Named vector showing the dimensions of an FLR object.
#'
#' @export

fdim <- function(x)
{
  setNames(dim(x), names(dimnames(x)))
}
