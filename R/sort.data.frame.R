#' Sort Data Frame
#'
#' Sort data frame by one or more columns.
#'
#' @param x a data frame.
#' @param decreasing whether to reverse the sorting order.
#' @param by a vector containing column numbers or column names to sort by,
#'        negative means descending.
#' @param na.last whether \code{NA} values are treated as being last in the
#'        sorting order.
#' @param \dots ignored (S3 template).
#'
#' @return Data frame containing sorted data.
#'
#' @note
#' Based on an S News email from Xiangyang Liu, 7 Aug 1995.
#'
#' As pointed out in the \verb{R-FAQ}, \code{x[order(x$a,-x$b),]} can also be
#' used to sort by multiple columns. That approach, using the minus sign to
#' reverse the sort order, will not work for string variables unless they are
#' wrapped inside \code{rank}: \code{x[order(x$a,-rank(x$b)),]}.
#'
#' @export
#' @export sort.data.frame

sort.data.frame <- function(x, decreasing=FALSE, by=seq_along(x),
                            na.last=TRUE, ...)
{
  if(is.numeric(by))
  {
    ascending <- by>0
    by <- abs(by)
  }
  else
  {
    ascending <- rep(TRUE, length.out=length(by))
  }
  decreasing <- rep(decreasing, length.out=length(by))
  ascending <- ifelse(decreasing, !ascending, ascending)

  keys <- seq_len(nrow(x))
  rotate <- rev(keys)  # static

  for(i in rev(seq_along(by)))
  {
    if(ascending[i])
      keys <- keys[order(x[keys,by[i]], na.last=na.last)]
    else
      keys <- keys[order(x[keys,by[i]], rotate, na.last=na.last)[rotate]]
  }
  output <- x[keys,, drop=FALSE]

  output
}
