#' Nano Edit in Place
#'
#' Edit function in place, using the Nano editor.
#'
#' @param \dots passed to \code{eip}.
#'
#' @note
#' To make fast edits without invoking Emacs, when working on a remote Linux
#' machine.
#'
#' @return
#' Invisible copy of the updated function.
#'
#' @export

neip <- function(...)
{
  eip(..., editor="nano")
}
