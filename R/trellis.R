#' Trellis Device
#'
#' Open \code{trellis.device} with black and white defaults.
#'
#' @param color whether colors are displayed.
#' @param \dots passed to \code{trellis.device}.
#'
#' @return \code{NULL}, but a device is opened.
#'
#' @note Shorthand for \code{trellis.device(color=FALSE, ...)}.
#'
#' @importFrom lattice trellis.device
#'
#' @export

trellis <- function(color=FALSE, ...)
{
  trellis.device(color=color, ...)
}
