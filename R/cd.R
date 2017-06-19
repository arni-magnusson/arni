#' Change Directory
#'
#' Set the working directory.
#'
#' @param dir an existing directory.
#'
#' @return
#' Working directory before the change.
#'
#' @note
#' Shorthand for \code{setwd()} for quick navigation in the console.
#'
#' @export

cd <- function(dir)
{
  setwd(dir)
}
