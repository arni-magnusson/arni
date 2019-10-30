#' Sum of Squares
#'
#' Calculate sum of squared residuals from the mean (not RSS).
#'
#' @param x a numeric vector.
#' @param na.rm whether \code{NA} values should be stripped.
#'
#' @return Sum of squared residuals from the mean, as a single number.
#'
#' @note
#' \code{RSS = ss0(resid)}, not \code{ssmean(resid)}.
#'
#' Similar speed in a benchmark with 1e5 calls of length 100:
#' \enumerate{
#' \item \code{sumx<-sum(x); output<-sum(x*x)-sumx*sumx/length(x)}
#' \item \code{output<-.Internal(cov(x,NULL,1,FALSE))*(length(x)-1)}, which is
#'       the same as \code{var(x)*(length(x)-1)}
#' \item \code{x_xbar<-x-.Internal(mean(x)); output<-sum(x_xbar*x_xbar)}
#' }
#'
#' These are all 3 x faster than \code{output<-sum((x-mean(x))^2)}.
#'
#' Implementation 1 is used here, since the algorithm can be used in other
#' languages.
#'
#' @export

ssmean <- function (x, na.rm=FALSE)
{
  if(na.rm)
    x <- x[!is.na(x)]

  sumx <- sum(x)

  sum(x*x) - sumx*sumx/length(x)
}
