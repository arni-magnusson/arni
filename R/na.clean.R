#' Remove NA Values
#'
#' Remove all elements (if vector) or rows (if matrix/data frame) containing
#' \code{NA} values.
#'
#' @param x vector or matrix/data frame to check for \code{NA} values.
#'
#' @return
#' Object similar to \code{x}, but possibly shorter (if vector) or fewer rows
#' (if matrix/data frame).
#'
#' @note
#' Similar to \code{na.exclude} and \code{na.omit}, but without special
#' attributes.
#'
#' @export

na.clean <- function(x)
{
  if(is.null(dim(x)))  # vector
  {
    output <- x[!is.na(x)]
  }
  else
  {
    na <- apply(is.na(x), 1, any)
    output <- x[!na,]
  }

  output
}
