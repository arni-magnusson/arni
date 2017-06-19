#' Relative Change
#'
#' Calculate relative change as a fraction of original value.
#'
#' @param from the original value, or a vector of length 2.
#' @param to the new value, or \code{NULL} when \code{from} is a vector of
#'        length 2.
#'
#' @return
#' Relative change between \code{-1} and \code{Inf}, where \code{0} means no
#' change.
#'
#' @export

change <- function(from, to=NULL)
{
  if(length(from) == 2)
  {
    to <- from[2]
    from <- from[1]
  }

  (to - from) / from
}
