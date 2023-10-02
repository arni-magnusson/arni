#' Normal Distribution Test
#'
#' Test whether sample is normal distributed.
#'
#' @param x a numeric vector.
#' @param plot whether to plot histogram and overlay norm distribution.
#' @param dots the number of dots in plotted normal distribution.
#' @param verbose whether to return expected curve as a data frame containing
#'        \code{x} and \code{y}.
#' @param pch point character.
#' @param cex.points point size.
#' @param \dots passed to \code{hist}.
#'
#' @return Test result as \code{htest} object.
#'
#' @note
#' Shapiro-Wilk test is used, as it seems to be preferred over the traditional
#' G-test and Kolmogorov-Smirnov test.
#'
#' @importFrom graphics hist points lines
#' @importFrom stats shapiro.test
#'
#' @export

norm.test <- function(x, plot=TRUE, dots=50, verbose=FALSE, pch=16,
                      cex.points=1, ...)
{
  x <- x[!is.na(x)]

  if(plot)
  {
    hist.object <- hist(x, ...)
    n <- length(x)
    mu <- mean(x)
    sigma <- sd(x)
    breaks <- hist.object$breaks
    bar.width <- diff(breaks)[1]
    xdots <- seq.int(min(breaks), max(breaks), length.out=dots)
    ydots <- n * (pnorm(xdots+1/2*bar.width,mu,sigma) -
                      pnorm(xdots-1/2*bar.width,mu,sigma))
    lines(xdots, ydots)
    points(hist.object$mids, hist.object$counts, pch=pch, cex=cex.points)
  }
  test.result <- shapiro.test(x)

  output <- if(verbose) data.frame(x=xdots,y=ydots) else test.result

  output
}
