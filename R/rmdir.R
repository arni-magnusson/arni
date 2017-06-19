#' Remove Empty Directories
#'
#' Remove empty directories under any operating system.
#'
#' @param path a directory name.
#' @param recursive whether to remove empty subdirectories as well.
#'
#' @note
#' To tidy up a project directory, it can be useful to remove empty
#' subdirectories, while leaving non-empty directories intact.
#'
#' The base function \code{unlink(dir, recursive=FALSE)} does not remove empty
#' directories in Windows and \code{unlink(dir, recursive=TRUE)} removes
#' non-empty directories, making it unsuitable for tidying up empty ones.
#'
#' @return
#' \code{TRUE} for success, \code{FALSE} for failure, invisibly.
#'
#' @export

rmdir <- function(path, recursive=FALSE)
{
  if(recursive)
  {
    paths <- rev(c(path, dir(path, full.names=TRUE, recursive=TRUE,
                             include.dirs=TRUE)))
    code <- sapply(paths, rmdir, recursive=FALSE)
  }
  else
  {
    ## Not an existing directory
    if(!dir.exists(path))
    {
      code <- FALSE
    }
    ## Not an empty directory
    else if(length(dir(path,all.files=TRUE,no..=TRUE)) > 0)
    {
      code <- FALSE
    }
    ## Existing and empty
    else
    {
      unlink(path, recursive=TRUE)
      code <- TRUE
    }
  }
  invisible(code)
}
