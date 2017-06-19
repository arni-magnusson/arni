#' Normalize
#'
#' Normalize vector to a dimensionless scale.
#'
#' @param x a numeric vector.
#' @param method either \code{"uniform"} (range 0-1), \code{"student"} (mean 0,
#'        sd 1), or \code{"relative"} (mean 1).
#'
#' @note
#' For example, \code{c(10,20,30)} becomes:
#' \tabular{llll}{
#'   uniform  \tab 0   \tab 0.5 \tab 1\cr
#'   student  \tab -1  \tab 0   \tab 1\cr
#'   relative \tab 0.5 \tab 1   \tab 1.5
#' }
#'
#' @seealso
#' \code{\link{scale}}, equivalent to \code{method = "student"}.
#'
#' @return
#' Numeric vector.
#'
#' @export

normalize <- function(x, method="uniform")
{
  method <- match.arg(method, c("uniform","relative","student"))

  if(method == "uniform")
    y <- (x-min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
  else if(method == "student")
    y <- (x-mean(x,na.rm=TRUE)) / sd(x,na.rm=TRUE)
  else if(method == "relative")
    y <- x / mean(x,na.rm=TRUE)

  y
}
