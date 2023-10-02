#' FLR Dimensions
#'
#' Show dimensions of an FLR object.
#'
#' @param x FLR object.
#'
#' @return Named vector showing the dimensions of an FLR object.
#'
#' @importFrom methods is
#' @importFrom stats setNames
#'
#' @export

fdim <- function(x)
{
  if(is(x, "FLQuant"))
    setNames(dim(x), names(dimnames(x)))
  else
    sapply(x, function(col) length(unique(col)))[-ncol(x)]
}
