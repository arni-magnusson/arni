#' Month Number
#'
#' Extract month number from a date object.
#'
#' @param x a vector of dates, of class \code{Date} or \code{POSIXt}.
#'
#' @return Vector of months as integers.
#'
#' @note Use \code{months} to extract month name instead.
#'
#' @export

monthsInt <- function(x)
{
  as.integer(format(x, "%m"))
}
