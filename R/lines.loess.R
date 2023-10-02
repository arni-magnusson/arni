#' Loess Line
#'
#' Add fitted loess line to an existing plot.
#'
#' @param x a fitted loess model.
#' @param \dots passed to \code{lines}.
#'
#' @return \code{NULL}, but line is added to plot.
#'
#' @importFrom graphics lines
#'
#' @export
#' @export lines.loess

lines.loess <- function(x, ...)
{
  model <- x
  x <- model$x
  fit <- model$fit
  lines(x[order(x)], fit[order(x)], ...)
}
