#' Number to Octal
#'
#' Convert number to octal string.
#'
#' @param x a number.
#' @param width minimum width for output string.
#'
#' @return
#' Octal string.
#'
#' @export

num2oct <- function(x, width=NULL)
{
  format.octmode(x, width=width)
}
