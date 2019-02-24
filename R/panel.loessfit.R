#' Scatterplot with Loess Line
#'
#' Panel function to draw a scatterplot with a fitted loess line.
#'
#' @param x x coordinates.
#' @param y y coordinates.
#' @param col.line line color.
#' @param col.points point color.
#' @param \dots passed to \code{panel.xyplot} and \code{panel.loess}.
#'
#' @return \code{NULL}, but draws a scatterplot panel with line.
#'
#' @importFrom lattice panel.loess panel.xyplot
#'
#' @export

panel.loessfit <- function(x, y, col.line="black", col.points="black", ...)
{
  panel.xyplot(x, y, col=col.points, ...)
  panel.loess(x, y, col=col.line, ...)
}
