#' Unicode
#'
#' Get character from the Unicode code chart (or get code point of a character
#' if \code{reverse = TRUE}).
#'
#' @param x a code point as decimal or hexadecimal number (or a character if
#'        \code{reverse = TRUE}).
#' @param reverse whether to get a point point from a character.
#'
#' @return String vector, or integer vector if \code{reverse = TRUE}.
#'
#' @note
#' Hexadecimal number can be passed as \code{0xfe} or \code{"fe"}, but not
#' \code{"0xfe"}.
#'
#' @export

unicode <- function(x=0:255, reverse=FALSE)
{
  if(reverse)
  {
    char <- x
    dec <- sapply(char, utf8ToInt, USE.NAMES=FALSE)
    output <- dec
  }
  else
  {
    dec <- if(is.character(x)) as.integer(paste0("0x",x)) else as.integer(x)
    char <- suppressWarnings(sapply(dec, intToUtf8, USE.NAMES=FALSE))
    output <- char
  }
  output
}
