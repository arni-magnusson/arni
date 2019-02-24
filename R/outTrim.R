#' Trim Outliers
#'
#' Trim extreme values from a vector.
#'
#' @param x a numeric vector.
#' @param fraction the fraction (0 to 0.5) of observations to be trimmed from
#'        each end of \code{x}.
#' @param na.rm whether \code{NA} values should be removed from vector before
#'        trimming.
#'
#' @return Sorted vector, same size or shorter than \code{x}.
#'
#' @importFrom stats median
#'
#' @export

outTrim <- function(x, fraction=0, na.rm=FALSE)
{
  if(any(is.na(x)) && !na.rm)
    stop("x contains NA values, maybe you want to pass na.rm=TRUE")
  if(any(is.na(x)) && na.rm) x <- x[!is.na(x)]

  n <- length(x)
  if(fraction <= 0)
    y <- x
  else if(fraction >= 0.5)
    y <- median(x)
  else
  {
    lo <- floor(n*fraction) + 1
    hi <- n + 1 - lo
    y <- sort(x)[lo:hi]
  }

  y
}
