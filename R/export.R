#' Export Data
#'
#' Print object without quotes or row names.
#'
#' @param x a vector, matrix, table, or data frame.
#'
#' @note
#' Useful for copying data from the console to a text editor or spreadsheet
#' without \verb{[1,]} indicators.
#'
#' @return
#' Data frame representation of \code{x}.
#'
#' @export

export <- function(x)
{
  if(is.null(dim(x)))
    x <- data.frame(" "=x, check.names=FALSE)
  if(is.matrix(x))
    x <- as.data.frame(x)
  if(is.table(x))
    x <- as.data.frame(unclass(x))

  print(x, row.names=FALSE)
  invisible(x)
}
