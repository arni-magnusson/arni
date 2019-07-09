#' Source Files
#'
#' Source files from a directory.
#'
#' @param directory a directory containing R source files.
#' @param pattern passed to \code{dir} when selecting files.
#' @param all.files passed to \code{dir} when selecting files.
#' @param recursive passed to \code{dir} when selecting files.
#' @param envir an environment where sourced objects should be assigned.
#' @param keep.source whether to retain source code format and comments.
#' @param quiet whether to suppress showing names of sourced files.
#' @param \dots passed to \code{sys.source} when sourcing files.
#'
#' @return Names of sourced files.
#'
#' @export

Source <- function(directory, pattern="\\.R$", all.files=FALSE, recursive=FALSE,
                   envir=parent.frame(), keep.source=getOption("keep.source"),
                   quiet=FALSE, ...)
{
  files <- dir(directory, full.names=TRUE, pattern=pattern, all.files=all.files,
               recursive=recursive)

  sapply(files, function(f)
  {
    if(!quiet)
      cat("  ", f, "\n", sep="")
    sys.source(f, envir=envir, keep.source=keep.source, ...)
  })

  invisible(files)
}
