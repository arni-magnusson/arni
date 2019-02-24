#' Update Packages
#'
#' Update packages in user library.
#'
#' @param lib.loc library to update.
#' @param \dots passed to \code{update.packages}.
#'
#' @return \code{NULL}, but packages are updated.
#'
#' @note Shorthand for \code{update.packages(.libPaths()[1])}.
#'
#' @importFrom utils update.packages
#'
#' @export

u <- function(lib.loc=.libPaths()[1], ...)
{
  update.packages(lib.loc=lib.loc, ...)
}
