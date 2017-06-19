#' HCL Colors
#'
#' Plot HCL colors as a wheel with \code{c=120} chroma and \code{l=85}
#' luminance.
#'
#' @param n the number of hue values.
#' @param col an optional set of colors, to show some other palette.
#' @param \dots passed to \code{hsvDemo}.
#'
#' @return
#' \code{NULL}, but a plot is drawn.
#'
#' @seealso
#' \code{\link{hsvDemo}}, \code{\link{rgbDemo}}.
#'
#' @importFrom grDevices hcl
#'
#' @export

hclDemo <- function(n=400, col=hcl(360*seq(0,1-1/n,1/n),c=120), ...)
{
  hsvDemo(col=col, ...)
}
