#' List Objects
#'
#' Show names of objects or elements.
#'
#' @param \dots passed to \code{ls}.
#'
#' @return Names of objects or elements, as \code{noquote} vector.
#'
#' @importFrom gdata ll
#'
#' @export

l <- function(...)
{
  noquote(row.names(ll(...)))
}
