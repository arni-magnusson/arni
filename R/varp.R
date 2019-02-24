#' Variance in Population
#'
#' Calculate variance based on entire population.
#'
#' @param x a numeric vector or the like.
#' @param na.rm whether \code{NA} values should be stripped.
#'
#' @return Variance, as a single number.
#'
#' @note Useful to evaluate MLE of \eqn{\sigma^2}.
#'
#' @export

varp <- function(x, na.rm=FALSE)
{
  if(na.rm)
    x <- x[!is.na(x)]
  ss <- ssmean(x)
  n <- length(x)

  ss / n
}
