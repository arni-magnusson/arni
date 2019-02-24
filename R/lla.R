#' List All Objects
#'
#' Show information about objects or elements, including hidden ones.
#'
#' @param \dots passed to \code{ls}.
#'
#' @return
#' Data frame with named rows and the following columns:
#' \preformatted{
#'   Class  object class
#'   KB     object size (name of this column is same as unit used)
#'   Dim    object dimensions
#' }
#'
#' @note Shorthand for \code{ll(all=TRUE)}.
#'
#' @importFrom gdata ll
#'
#' @export

lla <- function(...)
{
  ll(..., all.names=TRUE)
}
