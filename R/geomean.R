#' Geometric Mean
#'
#' Calculate geometric mean.
#'
#' @param x a numeric vector.
#' @param na.rm whether \code{NA} values should be stripped before the
#'   computation proceeds.
#'
#' @return
#' Geometric mean as a vector.
#'
#' @export

geomean <- function(x, na.rm=FALSE)
{
  exp(mean.default(log(x),na.rm=na.rm))
}
