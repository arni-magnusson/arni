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
#' x <- system.file("DESCRIPTION", package="base")
#' y <- system.file("DESCRIPTION", package="datasets")
#' diff(x, y)
#'
#' @export
#' @export diff.character

diff.character <- function(x, y, ...)
{
  A <- readLines(x)
  B <- readLines(y)
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  out <- list(diffA, diffB)
  names(out) <- c(x, y)
  out
}
