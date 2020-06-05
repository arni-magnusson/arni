#' Update Packages
#'
#' Update packages.
#'
#' @param \dots passed to \code{update.packages}.
#'
#' @note Shorthand notation for \code{update.packages()}.
#'
#' @importFrom utils update.packages
#'
#' @examples
#' \dontrun{
#' u()
#' u(libPaths()[1])  # skip core packages
#' }
#'
#' @export

u <- function(...)
{
  update.packages(...)
}
