#' Trellis Device
#'
#' Open \code{trellis.device} with black and white defaults.
#'
#' @param color whether colors are displayed.
#' @param \dots passed to \code{trellis.device}.
#'
#' @note
#' Shorthand for \code{trellis.device(color=FALSE, ...)}.
#'
#' @return
#' \code{NULL}, but a device is opened.
#'
#' @importFrom lattice trellis.device
#'
#' @export

trellis <- function(color=FALSE, ...)
{
  trellis.device(color=color, ...)
}
