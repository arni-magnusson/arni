#' Single Quantile Plot
#'
#' Single quantile plot, or a scatterplot matrix with quantile plots diagonal
#' panels.
#'
#' @param x is a vector, matrix, or data frame.
#' @param xlab x axis label (only used if \code{x} is a vector).
#' @param ylab y axis label (only used if \code{x} is a vector).
#' @param \dots passed to plot and pairs.
#'
#' @return Invisible fractions, ordered from 0 to 1.
#'
#' @note Somewhat similar to \code{plot.data.frame} in S-Plus.
#'
#' @importFrom graphics pairs
#'
#' @export

splot <- function(x, xlab=NULL, ylab=NULL, ...)
{
  if(is.list(x) || is.matrix(x))
  {
    x <- as.data.frame(x)
    panel.sort <- function(x, ...)
    {
      par(new=TRUE)
      p <- seq(0, 1, length.out=length(x))
      plot(p, sort(x), axes=FALSE, ...)
    }
    pairs(x, diag.panel=panel.sort, ...)
    p <- seq(0, 1, length.out=nrow(x))
  }
  else
  {
    if(is.null(xlab))
      xlab <- "Fraction"
    if(is.null(ylab))
      ylab <- deparse(substitute(x))
    p <- seq(0, 1, length.out=length(x))
    plot(p, sort(x), xlab=xlab, ylab=ylab, ...)
  }

  invisible(p)
}
