#' Right Substring
#'
#' Get the right part of a string.
#'
#' @param string the whole string.
#' @param chars the minimum substring length.
#'
#' @return
#' String containing the right part of \code{string} or the whole string.
#'
#' @export

right <- function(string, chars)
{
  substring(string, first=nchar(string)-chars+1, last=nchar(string))
}
