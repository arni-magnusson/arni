#' Average Angle
#'
#' Calculate average angle.
#'
#' @param h a vector of angles, between 0 and 1 (0 is up and 0.25 is right).
#' @param w a vector of weights (arbitrary scale).
#' @param digits passed to \code{round}, to make sure that
#'        \code{avgAngle(c(0,0.5))} returns \code{NA}.
#'
#' @return Average angle as a number.
#'
#' @note
#' There are several ways to define average angle; this one uses the average of
#' Cartesian coordinates.
#'
#' Possible applications: current or tow direction, time of year, time of day,
#' color hue.
#'
#' @examples
#' # Average of NNW and NNE is north
#' avgAngle(c(0.875, 0.125))
#'
#' @export

avgAngle <- function(h, w=1, digits=getOption("digits"))
{
  ## Geometric angle (0 is right and pi/2 is up)
  a <- (-2*pi*h + 2.5*pi) %% (2*pi)

  x <- cos(a) * w
  y <- sin(a) * w

  xbar <- round(mean(x), digits)
  ybar <- round(mean(y), digits)
  abar <- if(xbar!=0 || ybar!=0) atan2(ybar, xbar) else NA

  hbar <- (-0.5*abar/pi + 1.25) %% 1

  hbar
}
