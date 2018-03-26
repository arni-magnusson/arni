#' Normalize
#'
#' Normalize vector to a dimensionless scale.
#'
#' @param x a numeric vector.
#' @param method either \code{"uniform"} (range 0-1), \code{"student"} (mean 0,
#'        sd 1), or \code{"relative"} (mean 1, original cv).
#'
#' @note
#' For example, \code{c(10,20,30)} becomes:
#' \preformatted{
#' uniform    0.0  0.5  1.0
#' student   -1.0  0.0  1.0
#' relative   0.5  1.0  1.5
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

  switch(
    method,
    uniform = (x-min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE)-min(x,na.rm=TRUE)),
    student = (x-mean(x,na.rm=TRUE)) / sd(x,na.rm=TRUE),
    relative = x / mean(x,na.rm=TRUE))
}
