#' Statistical Mode
#'
#' Get statistical mode, the most frequent value.
#'
#' @param x an object, usually a vector or matrix.
#' @param all whether to return all modes (default returns only first).
#' @param \dots passed to \code{table}.
#'
#' @return
#' The most frequent value in \code{x}, possibly a vector if \code{all = TRUE}.
#'
#' @importFrom methods as
#'
#' @export

statmode <- function(x, all=FALSE, ...)
{
  if(is.list(x))
  {
    output <- sapply(x, statmode, all=all, ...)
  }
  else
  {
    freq <- table(x, ...)
    if(all)
      output <- names(freq)[freq==max(freq)]
    else
      output <- names(freq)[which.max(freq)]
    ## Coerce to original data type, using any() to handle mts, xtabs, etc.
    if(any(class(x) %in%
           c("integer","numeric","ts","complex","matrix","table")))
      output <- as(output, storage.mode(x))
  }
  output
}
