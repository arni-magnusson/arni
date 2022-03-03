#' Compare Files
#'
#' Show differences between files or folders.
#'
#' @param x a file or folder name.
#' @param y another file or folder name.
#' @param \dots ignored.
#'
#' @return List containing lines that are different between the files.
#'
#' @examples
#' \dontrun{
#' diff("this.txt", "that.txt")
#'
#' x <- system.file("DESCRIPTION", package="base")
#' y <- system.file("DESCRIPTION", package="datasets")
#' diff(x, y)
#' }
#'
#' @export
#' @export diff.character

diff.character <- function(x, y, ...)
{
  ## 1  Read files
  A <- readLines(x)
  B <- readLines(y)

  ## 2  Compare
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  out <- list(diffA, diffB)
  names(out) <- c(x, y)

  ## 3  Replace character(0) with NULL
  out[sapply(out, length) == 0] <- NULL
  if(length(out) == 0)
  {
    out <- NULL
    invisible(out)
  }
  else
  {
    out
  }
}
