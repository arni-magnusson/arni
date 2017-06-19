#' Latin-1
#'
#' Get character from the Latin-1 code chart (or get code point of a character
#' if \code{reverse = TRUE}).
#'
#' @param x a code point as decimal or hexadecimal number (or a character if
#'        \code{reverse = TRUE}).
#' @param reverse whether to get code point of a character.
#'
#' @note
#' Hexadecimal number can be passed as \code{0xfe} or \code{"fe"}, but not
#' \code{"0xfe"}.
#'
#' @return
#' String vector, or integer vector if \code{reverse = TRUE}.
#'
#' @export

latin1 <- function(x=0:255, reverse=FALSE)
{
  if(reverse)
  {
    char <- as.character(x)
    dec <- as.integer(sapply(char, charToRaw, USE.NAMES=FALSE))
    output <- dec
  }
  else
  {
    dec <- if(is.character(x)) as.integer(paste0("0x",x)) else as.integer(x)
    char <- suppressWarnings(sapply(as.raw(dec), rawToChar, USE.NAMES=FALSE))
    Encoding(char) <- "latin1"
    output <- char
  }
  output
}
