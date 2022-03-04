#' Compare Files
#'
#' Show differences between files or folders.
#'
#' @param x a file or folder name.
#' @param y another file or folder name.
#' @param file if \code{x} and \code{y} are folders, then \code{file} can be
#'        used to select a specific file that exists in both folders.
#' @param lines if \code{x} and \code{y} are folders, then \code{lines}
#'        indicates whether to compare file contents (lines) instead of
#'        filenames.
#' @param simplify is whether to replace \code{character(0)} with \code{NULL} in
#'        output, for more compact display of results.
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

diff.character <- function(x, y, file=NULL, lines=FALSE, short=TRUE, simplify=TRUE, ...)
{
  if(dir.exists(x) && dir.exists(y))
  {
    if(is.null(file))
    {
      if(lines)
      {
        files <- intersect(dir(x), dir(y))
        out <- mapply(diff.file, file.path(x, files), file.path(y, files),
                      simplify=simplify)
        names(out) <- files
        out
      }
      else
      {
        diff.dir(x, y, simplify=simplify)
      }
    }
    else
    {
      diff.file(file.path(x, file), file.path(y, file), simplify=simplify)
    }
  }
  else if(file.exists(x) && file.exists(y))
  {
    diff.file(x, y, simplify=simplify)
  }
  else
  {
    if(!file.exists(x))
      stop("'", x, "' not found")
    if(!file.exists(y))
      stop("'", y, "'not found")
  }
}

diff.dir <- function(x, y, simplify=TRUE)
{
  ## 1  Read directories
  A <- dir(x)
  B <- dir(y)

  ## 2  Compare
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  out <- list(diffA, diffB)
  names(out) <- short.path(x, y)

  ## 3  Replace character(0) with NULL
  if(simplify)
  {
    out[sapply(out, length) == 0] <- NULL
    if(length(out) == 0)
      out <- NULL
  }

  out
}

diff.file <- function(x, y, simplify=TRUE)
{
  ## 1  Read files
  A <- readLines(x)
  B <- readLines(y)

  ## 2  Compare
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  out <- list(diffA, diffB)
  names(out) <- short.path(x, y)

  ## 3  Replace character(0) with NULL
  if(simplify)
  {
    out[sapply(out, length) == 0] <- NULL
    if(length(out) == 0)
      out <- NULL
  }

  out
}

short.path <- function(A, B)
{
  ## 1  Convert \\ to /
  A <- gsub("\\\\", "/", A)
  B <- gsub("\\\\", "/", B)

  ## 2  Exit if same (this only happens if original A and B are identical)
  if(A == B)
    c(A, B)  # this only happens if original A and B are identical
  else if(basename(A) != basename(B))
    c(basename(A), basename(B))  # x/y/A.txt and x/y/B.txt => A.txt and B.txt
  else
    short.path(dirname(A), dirname(B))  # x/A/y/n.txt and x/B/y/n.txt => A and B
}
