#' Get Anywhere
#'
#' Show non-visible function.
#'
#' @param \dots a function or function name.
#'
#' @return Shows function.
#'
#' @note Shorthand notation for \code{getAnywhere()}.
#'
#' @importFrom utils getAnywhere
#'
#' @export

ga <- function(...)
{
  ## If argument is called 'x' instead of '...' then
  ## ga(foo) would look for a function called x()
  getAnywhere(...)
}
