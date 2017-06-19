#' Month Number
#'
#' Extract month number from a date object.
#'
#' @param x a vector of dates, of class \code{Date} or \code{POSIXt}.
#'
#' @note
#' Use \code{months} to extract month name instead.
#'
#' @return
#' Vector of months as integers.
#'
#' @export

monthsInt <- function(x)
{
  as.integer(format(x, "%m"))
}
