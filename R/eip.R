#' Edit in Place
#'
#' Edit function in place.
#'
#' @param name a function, quoted or unquoted.
#' @param \dots passed to \code{edit}.
#'
#' @note
#' Based on \code{eip} in package \pkg{Hmisc}.
#'
#' @return
#' A copy of the updated function.
#'
#' @importFrom utils edit find
#'
#' @export

eip <- function(name, ...)
{
  name <- as.character(substitute(name))
  f <- find(name)
  if(length(f) != 1)
    stop("Object must exist in exactly one place")

  g <- edit(get(name), ...)
  assign(name, g, pos=f)
  cat("Object", name, "stored in", f, "\n")

  invisible(g)
}
