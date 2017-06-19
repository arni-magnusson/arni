#' Rename Object
#'
#' Rename object in workspace.
#'
#' @param from object to rename (quoted or unquoted).
#' @param to new object name (quoted or unquoted).
#' @param sure whether existing object may be overwritten.
#' @param quiet whether report should be suppressed.
#'
#' @return
#' \code{NULL}, but the object is renamed.
#'
#' @export

mv <- function (from, to, sure=FALSE, quiet=TRUE)
{
  from <- as.character(substitute(from))
  to <- as.character(substitute(to))
  if(!exists(from,where=1,inherits=FALSE))
  {
    stop("Object '", from, "' not found in workspace.")
  }
  else if(exists(to,where=1,inherits=FALSE) && !sure)
  {
    stop("Object '", to, "' already exists. Pass sure=TRUE to overwrite.")
  }
  else
  {
    assign(to, get(from,pos=1), pos=1)
    rm(list=from, pos=1)
    if(!quiet) cat("Object '", from, "' has been renamed to '", to, "'.\n",
                   sep="")
  }
  invisible(NULL)
}
