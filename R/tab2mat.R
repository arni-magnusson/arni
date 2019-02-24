#' Table to Matrix
#'
#' Convert table (including xtabs) to matrix.
#'
#' @param x a table.
#'
#' @return Matrix with same data as \code{x}.
#'
#' @export

tab2mat <- function(x)
{
  as.matrix(as.data.frame(unclass(x)))
}
