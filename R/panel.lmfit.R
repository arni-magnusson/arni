#' Scatterplot with Least-Squares Line
#'
#' Panel function to draw scatterplot with least-squares line.
#'
#' @param x x coordinates.
#' @param y y coordinates.
#' @param col.line line color.
#' @param col.points point color.
#' @param \dots passed to \code{panel.xyplot} and \code{panel.lmline}.
#'
#' @return
#' \code{NULL}, but draws a scatterplot panel with line.
#'
#' @importFrom lattice panel.lmline panel.xyplot
#'
#' @export

panel.lmfit <- function(x, y, col.line="black", col.points="black", ...)
{
  panel.xyplot(x, y, col=col.points, ...)
  panel.lmline(x, y, col=col.line, ...)
}
