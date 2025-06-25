#' Standard Deviation in Population
#'
#' Calculate standard deviation based on entire population.
#'
#' @param x a numeric vector or the like.
#' @param na.rm whether \code{NA} values should be stripped.
#'
#' @return Standard deviation, as a single number.
#'
#' @note Useful to evaluate MLE of \eqn{\sigma}.
#'
#' @export

sdp <- function(x, na.rm=FALSE)
{
  if(na.rm)
    x <- x[!is.na(x)]

  ss <- ssmean(x)
  n <- length(x)

  sqrt(ss/n)
}
