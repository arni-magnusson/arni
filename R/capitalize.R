#' Capitalize Words
#'
#' Make every word in string start in uppercase.
#'
#' @param x a string (one or more words) to be formatted.
#' @param strict whether existing uppercase letters should be lowercased.
#'
#' @note
#' Based on example in \code{help(chartr)}. Better for multi-word strings than
#' \code{Hmisc::capitalize}.
#'
#' @return
#' String with every word starting in uppercase.
#'
#' @seealso
#' \code{\link{toTitleCase}} does not capitalize all words.
#'
#' @export

capitalize <- function(x, strict=TRUE)
{
  cap <- function(x)
  {
    first <- toupper(substring(x,1,1))
    rest <- if(strict) tolower(substring(x,2)) else substring(x,2)
    output <- paste0(first, rest, collapse=" ")
  }
  sapply(strsplit(x,split=" "), cap, USE.NAMES=!is.null(names(x)))
}
