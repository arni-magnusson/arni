#' Binary to Decimal
#'
#' Convert binary number to decimal.
#'
#' @param x a binary string, like \code{"101"}.
#'
#' @return Integer.
#'
#' @export

bin2num <- function(x)
{
  strtoi(x, 2)
}
