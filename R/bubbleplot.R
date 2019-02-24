#' Bubble Plot
#'
#' Draw bubble plot, a scatterplot with varying symbol sizes and colors.
#'
#' @param x a vector of values for the horizontal axis, or a data frame
#'        containing \code{x}, \code{y}, and \code{z} in that order.
#' @param y a vector of values for the vertical axis.
#' @param z a vector of values determining the bubble sizes.
#' @param std whether to standardize \code{z} by dividing with \code{mean(z)}.
#' @param pow a power coefficient for the bubble sizes (\code{0.5} is
#'        \code{sqrt}).
#' @param add whether to add bubbles to existing plot.
#' @param rev whether to reverse the y axis.
#' @param cex.points scales all bubble sizes.
#' @param type passed to \code{points}.
#' @param ylim passed to \code{plot}.
#' @param xlab passed to \code{plot}.
#' @param ylab passed to \code{plot}.
#' @param pch passed to \code{points}.
#' @param col passed to \code{points}.
#' @param bg passed to \code{points}.
#' @param \dots passed to \code{bubbleplot.*} methods.
#' @param formula has the form \code{z~x+y}, where \code{z} determines the
#'        bubble sizes and \code{x} and \code{y} determine bubble locations.
#' @param data where formula terms are stored, e.g. data frame or list.
#' @param subset a logical vector specifying which data to plot.
#' @param na.action how \code{NA} values are handled.
#'
#' @return \code{NULL}, but a bubble plot is drawn.
#'
#' @note
#' Negative \code{z} values are drawn in \code{col[2]} and \code{bg[2]}.
#'
#' @importFrom graphics par points
#'
#' @export

bubbleplot <- function(x, ...)
{
  UseMethod("bubbleplot")
}

#' @rdname bubbleplot
#' @export
#' @export bubbleplot.default

bubbleplot.default <- function(x, y, z, std=TRUE, pow=0.5, add=FALSE, rev=FALSE,
                               type="p", ylim=NULL, xlab=NULL, ylab=NULL,
                               pch=c(16,1), cex.points=1, col="black",
                               bg=par("bg"), ...)
{
  pch <- rep(pch, length.out=2)
  col <- rep(col, length.out=2)
  bg <- rep(bg, length.out=2)

  if(is.list(x))  # data.frame or list
  {
    if(is.null(xlab))
      xlab <- names(x)[1]
    if(is.null(ylab))
      ylab <- names(x)[2]
    y <- x[[2]]
    z <- x[[3]]
    x <- x[[1]]
  }

  if(is.null(ylim))
    ylim <- range(y)
  if(rev)
    ylim <- rev(ylim)
  if(is.null(xlab))
    xlab <- deparse(substitute(x))
  if(is.null(ylab))
    ylab <- deparse(substitute(y))

  if(std)
    mycex <- cex.points * (abs(z)/mean(abs(z)))^pow
  else
    mycex <- cex.points * abs(z)^pow

  if(!add)
    suppressWarnings(plot(x, y, type="n", ylim=ylim, xlab=xlab, ylab=ylab, ...))
  suppressWarnings(points(x[z>0], y[z>0], type=type, pch=pch[1], cex=mycex[z>0],
                          col=col[1], bg=bg[1], ...))
  suppressWarnings(points(x[z<0], y[z<0], type=type, pch=pch[2], cex=mycex[z<0],
                          col=col[2], bg=bg[2], ...))
}

#' @rdname bubbleplot
#' @export
#' @export bubbleplot.formula

bubbleplot.formula <- function(formula, data, subset, na.action=NULL, ...)
{
  m <- match.call(expand.dots=FALSE)
  if(is.matrix(eval(m$data,parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m$na.action <- na.action
  m[[1L]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())

  bubbleplot.default(mf[c(2,3,1)], ...)
}
