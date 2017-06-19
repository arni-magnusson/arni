#' Nano Editor
#'
#' Edit function with the Nano editor.
#'
#' @param \dots is passed to \code{fix}.
#'
#' @note
#' To make fast edits without invoking Emacs, when working on a remote Linux
#' machine.
#'
#' @return
#' Invisible copy of the updated function.
#'
#' @importFrom utils fix
#'
#' @export

nano <- function(...)
{
  fix(..., editor="nano")
}
