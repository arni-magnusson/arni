#' Uncrosstab
#'
#' Coerce crosstab table into three column data frame.
#'
#' @param x a crosstab table (data frame, matrix, or table), with important row
#'        names and column names.
#' @param labels a vector of three strings, used as column names for the
#'        resulting data frame.
#'
#' @return Data frame (3 cols) that can be crosstabbed to look like \code{x}.
#'
#' @note
#' Use dimnames like \code{"1990"} instead of \code{"Year1990"} for numeric
#' entries in resulting data frame.
#'
#' Another approach is \code{as.data.frame(as.table(as.matrix(x)))}.
#'
#' @export

unxtab <- function(x, labels=c(rows="Year",cols="Age",vals="Value"))
{
  r.count <- nrow(x)
  c.count <- ncol(x)
  r.names <- simplify(rownames(x))
  c.names <- simplify(colnames(x))

  col.1 <- rep(r.names, each=c.count)
  col.2 <- rep(c.names, times=r.count)
  col.3 <- as.vector(t(as.matrix(x)))

  output <- data.frame(col.1, col.2, col.3)
  names(output) <- labels

  output
}
