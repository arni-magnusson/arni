#' Difference between Two Times
#'
#' Calculate difference between two times in seconds.
#'
#' @param before a string like \code{"7:58:32"}.
#' @param after a string like \code{"8:55:09"}.
#'
#' @return Seconds as a number.
#'
#' @importFrom gmt deg2num
#'
#' @export

bench <- function(before, after)
{
  sec <- 3600 * (deg2num(after) - deg2num(before))

  sec
}
