#' Sum of Squares
#'
#' Calculate sum of squared values, e.g. RSS.
#'
#' @param x a numeric vector.
#' @param na.rm whether \code{NA} values should be stripped.
#'
#' @return Sum of squared values, as a single number.
#'
#' @note \code{RSS = ss0(resid)}, not \code{ssmean(resid)}.
#'
#' @export

ss0 <- function (x, na.rm=FALSE)
{
  if(na.rm)
    x <- x[!is.na(x)]
  sum(x*x)
}
