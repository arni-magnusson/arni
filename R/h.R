#' History
#'
#' Show full command history.
#'
#' @param max.show the number of previous commands to show (default is entire
#'   log).
#' @param reverse whether to show lines in reverse order (not useful when there
#'   are continuation lines).
#' @param \dots passed to \code{history}.
#'
#' @note
#' Shorthand notation for a common task.
#'
#' @return
#' Shows command history in external editor.
#'
#' @importFrom utils history
#'
#' @export

h <- function(max.show=Inf, reverse=TRUE, ...)
{
  history(max.show=max.show, reverse=reverse, ...)
}
