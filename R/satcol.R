#' Saturate Colors
#'
#' Adjust color saturation.
#'
#' @param col vector of colors (name, hexadecimal string, or integer).
#' @param s saturation level.
#'
#' @return Vector of colors as hexadecimal string.
#'
#' @importFrom grDevices col2rgb hsv rgb2hsv
#'
#' @export

satcol <- function(col, s=1)
{
  mHSV <- rgb2hsv(col2rgb(col))
  mHSV["s",] <- s
  sat <- do.call(hsv, as.data.frame(t(mHSV)))

  sat
}
