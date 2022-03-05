#' Nano Editor
#'
#' Edit function with the Nano editor.
#'
#' @param \dots passed to \code{fix}.
#'
#' @return Invisible copy of the updated function.
#'
#' @note
#' To make fast edits without invoking Emacs, when working on a remote Linux
#' machine.
#'
#' @importFrom utils fix
#'
#' @export

nano <- function(...)
{
  fix(..., editor="nano")
}
