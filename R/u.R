#' Update Packages
#'
#' Update packages.
#'
#' @param \dots passed to \code{update.packages}.
#'
#' @note
#' Shorthand for \code{update.packages}.
#'
#' @return
#' \code{NULL}, but packages are updated.
#'
#' @importFrom utils update.packages
#'
#' @export

u <- function(...)
{
  update.packages(...)
}
