#' Toggle Emacs
#'
#' Toggle whether Emacs is run with command line option \verb{-nw}.
#'
#' @note
#' Useful when working over a slow SSH connection.
#'
#' @return
#' Resulting \code{getOption("editor")} after adding or removing \verb{-nw}.
#'
#' @export

nw <- function()
{
  editor <- getOption("editor")
  already.nw <- grepl("-nw", editor)

  if(already.nw)
    editor <- gsub(" -nw", "", editor)  # remove -nw
  else
    editor <- gsub("emacs", "emacs -nw", editor) # add -nw

  options(editor=editor)
  cat(paste0("\"", editor, "\"\n"))

  invisible(editor)
}
