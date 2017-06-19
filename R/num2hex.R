#' Number to Hexadecimal
#'
#' Convert number to hexadecimal string.
#'
#' @param x a number.
#' @param width minimum width for output string.
#' @param upper whether to use upper case.
#'
#' @return
#' Hexadecimal string.
#'
#' @export

num2hex <- function(x, width=NULL, upper=FALSE)
{
  format.hexmode(x, width=width, upper.case=upper)
}
