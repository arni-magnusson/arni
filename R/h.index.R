#' h-index
#'
#' Calculate h-index.
#'
#' @param x a numeric vector.
#'
#' @return h-index.
#'
#' @export

h.index <- function(x)
{
  sortx <- sort(x, decreasing=TRUE)
  seqx <- seq_along(x)
  length(which(sortx >= seqx))
}
