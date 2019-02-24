#' Nano Edit in Place
#'
#' Edit function in place, using the Nano editor.
#'
#' @param \dots passed to \code{eip}.
#'
#' @return Invisible copy of the updated function.
#'
#' @note
#' To make fast edits without invoking Emacs, when working on a remote Linux
#' machine.
#'
#' @export

neip <- function(...)
{
  eip(..., editor="nano")
}
