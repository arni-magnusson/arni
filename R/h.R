#' History
#'
#' Show full command history.
#'
#' @param max.show the number of previous commands to show (default is entire
#'        log).
#' @param reverse whether to show lines in reverse order (not useful when there
#'        are continuation lines).
#' @param \dots passed to \code{history} and \code{file.show}.
#'
#' @note
#' Shorthand notation for \code{history()} with improved support for Emacs ESS.
#'
#' The base \code{history} function returns an error inside an Emacs session. As
#' a workaround, this function checks if it is run from inside Emacs and if that
#' is the case, it calls \code{file.show} to show the \file{.Rhistory} file
#' (ignoring both \code{max.show} and \code{reverse}).
#'
#' @importFrom utils history
#'
#' @export

h <- function(max.show=Inf, reverse=TRUE, ...)
{
  if(Sys.getenv("INSIDE_EMACS") == "")
  {
    history(max.show=max.show, reverse=reverse, ...)
  }
  else
  {
    r.histfile <- Sys.getenv("R_HISTFILE")
    file.show(if(r.histfile == "") ".Rhistory" else r.histfile, ...)
  }
}
