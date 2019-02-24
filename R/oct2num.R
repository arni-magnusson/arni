#' Octal to Number
#'
#' Convert octal string to number.
#'
#' @param x an octal string, like \code{"377"}.
#'
#' @return Integer.
#'
#' @export

oct2num <- function(x)
{
  strtoi(x, 8)
}
