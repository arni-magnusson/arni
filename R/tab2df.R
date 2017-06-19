#' Table to Data Frame
#'
#' Convert table (including xtabs) to data frame.
#'
#' @param x a table.
#'
#' @return
#' Data frame with same data as \code{x}.
#'
#' @export

tab2df <- function(x)
{
  as.data.frame(unclass(x))
}
