#' Uncrosstab
#'
#' Convert a table from crosstab format to long format.
#'
#' @param x a crosstab table (data frame, matrix, or table) with descriptive row
#'        names and column names.
#' @param labels a vector of three strings, used as column names for the
#'        resulting data frame.
#'
#' @return
#' Data frame in long format (3 columns) that can be crosstabbed to look like
#' \code{x}.
#'
#' @note
#' Use dimnames like \code{"1990"} instead of \code{"Year1990"} to end up with
#' numeric entries in resulting data frame.
#'
#' Another approach is \code{as.data.frame(as.table(as.matrix(x)))}.
#'
#' @export

unxtab <- function(x, labels=c("Year","Age","Value"))
{
  r.names <- simplify(rownames(x))
  c.names <- simplify(colnames(x))

  col.1 <- rep(r.names, each=ncol(x))
  col.2 <- rep(c.names, times=nrow(x))
  col.3 <- c(t(as.matrix(x)))

  output <- data.frame(col.1, col.2, col.3)
  names(output) <- labels

  output
}
