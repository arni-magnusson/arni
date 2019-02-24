#' Years
#'
#' Extract year from Date/POSIXt object.
#'
#' @param x a vector of dates, of class \code{Date} or \code{POSIXt}.
#'
#' @return Vector of years as integers.
#'
#' @export

years <- function(x)
{
  as.integer(format(x, "%Y"))
}
