#' Sort Data Frame
#'
#' Sort data frame by one or more columns.
#'
#' @param x a data frame.
#' @param decreasing ignored (S3 template).
#' @param by a vector containing column numbers or column names to sort by,
#'        negative means descending.
#' @param na.last whether \code{NA} values are treated as being last in
#'        alphabetical order.
#' @param \dots ignored (S3 template).
#'
#' @note
#' Based on an S News email from Xiangyang Liu, 7 Aug 1995.
#'
#' As pointed out in the \verb{R-faq.pdf}, \code{x[order(x$a,-x$b),]} can also
#' be used to sort by multiple columns.
#'
#' @return
#' Data frame containing sorted data.
#'
#' @export
#' @export sort.data.frame

sort.data.frame <- function(x, decreasing=NULL, by=seq_along(x),
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
