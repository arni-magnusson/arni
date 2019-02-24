#' Coefficient of Variation
#'
#' Calculate coefficient of variation.
#'
#' @param x a numeric vector.
#' @param na.rm whether \code{NA} values should be stripped.
#'
#' @return Coefficient of variation as a single number.
#'
#' @importFrom stats sd
#'
#' @export

cv <- function (x, na.rm=FALSE)
{
  sd(x,na.rm=na.rm) / abs(mean(x,na.rm=na.rm))
}
