#' Plot Loess
#'
#' Plot loess model fit as a line superposed on scatter.
#'
#' @param x a fitted loess model.
#' @param lwd the line width.
#' @param line.col the line colour.
#' @param xlab passed to \code{plot}.
#' @param ylab passed to \code{plot}.
#' @param \dots passed to \code{plot}.
#'
#' @return
#' \code{NULL}, but a plot is drawn.
#'
#' @importFrom stats terms
#'
#' @export

plot.loess <- function(x, lwd=2, line.col="black", xlab=NULL, ylab=NULL, ...)
{
  model <- x
  x   <- model$x
  y   <- model$y
  fit <- model$fit

  if(is.null(xlab))
    xlab <- as.character(terms(model))[3]
  if(is.null(ylab))
    ylab <- as.character(terms(model))[2]

  plot(x, y, xlim=range(x), ylim=c(min(y,fit),max(y,fit)), xlab=xlab, ylab=ylab,
       ...)
  lines(x[order(x)], fit[order(x)], lwd=lwd, col=line.col)

  invisible(NULL)
}
