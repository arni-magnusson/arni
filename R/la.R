#' List All Objects
#'
#' Show names of objects or elements, including hidden ones.
#'
#' @param \dots passed to \code{ls}.
#'
#' @return Names of objects or elements, as \code{noquote} vector.
#'
#' @note Shorthand for \code{l(all=TRUE)}.
#'
#' @export

la <- function(...)
{
  l(..., all.names=TRUE)
}
