#' Compare Files
#'
#' Show differences between files or folders.
#'
#' @param x a file or folder name.
#' @param y another file or folder name.
#' @param file if \code{x} and \code{y} are folders, then \code{file} can be
#'        used to select a specific file that exists in both folders.
#' @param lines if \code{x} and \code{y} are folders, then \code{lines = TRUE}
#'        compares the contents (lines) of files that exist in both folders,
#'        instead of listing filenames that are different between the folders.
#' @param short whether to produce short file paths for the output.
#' @param simple whether to replace \code{character(0)} with \code{NULL} in
#'        output, for compact display.
#' @param \dots passed to \code{readLines}.
#'
#' @details
#' When comparing folders, two kinds of differences can occur: (1) filenames
#' existing in one folder and not the other, and (2) files containing different
#' lines of text. The purpose of the \code{lines} argument is to select which of
#' those two kinds of differences to show.
#'
#' If \code{x} and \code{y} are files (and not folders), the \code{file} and
#' \code{lines} arguments are not applicable and will be ignored.
#'
#' @return List showing differences as strings.
#'
#' @note
#' File and folder names are of class \code{character} so \code{diff("pathA",
#' "pathB")} will dispatch this method to show the differences. There is no need
#' to call \code{diff.character} explicitly; a plain \code{diff} will do.
#'
#' This function uses \code{setdiff} for the comparison, so line order and line
#' numbers are ignored.
#'
#' Subfolders are excluded when comparing folders, but can be examined in
#' subsequent calls.
#'
#' This function has very basic features compared to full GUI applications such
#' as \emph{WinMerge} (Windows), \emph{Meld} (Linux, Windows), \emph{Kompare}
#' (Linux), \emph{Ediff} (Emacs), or the \command{diff} shell command. The use
#' of full GUI applications is recommended, but what this function offers in
#' addition is:
#'
#' \itemize{
#' \item a quick diff tool that is handy during an interactive R session,
#' \item a programmatic interface to analyze file differences as native R
#'       objects, and
#' \item a tool that works on all operating systems, regardless of what software
#'       may be installed, which can be practical for teamwork and online
#'       workshops.
#' }
#'
#' The default \code{short = TRUE} and \code{simple = TRUE} are designed for
#' interactive (human-readable) use, while \code{short = FALSE} and \code{simple
#' = FALSE} gives long-format output that can be useful for scripted analyses of
#' file differences.
#'
#' @seealso
#' \code{\link[base]{diff}} is a generic function. Depending on \code{x}, it
#' will show differences between numbers, date-time objects, files, folders,
#' etc.
#'
#' \code{\link{dir}}, \code{\link{readLines}}, and \code{\link{setdiff}} are the
#' underlying functions performing the file and folder comparison.
#'
#' The \pkg{diffr} package provides a visually effective comparison of two files
#' in HTML format. Unlike the \code{diff} function, however, \pkg{diffr} does
#' not compare folders or return differences as native R objects.
#'
#' @examples
#' \dontrun{
#'
#' # Compare two files
#' write(c("We", "are", "not"), file="one.txt")
#' write(c("We", "are", "the same"), file="two.txt")
#' diff("one.txt", "two.txt")
#' file.remove("one.txt", "two.txt")
#'
#' # Another example with two files
#' x <- system.file("DESCRIPTION", package="base")
#' y <- system.file("DESCRIPTION", package="datasets")
#' diff(x, y)
#'
#' # Compare two folders
#' x <- system.file(package="base")
#' y <- system.file(package="datasets")
#' diff(x, y)                       # these filenames are different
#' diffs <- diff(x, y, lines=TRUE)  # compare files that exist in both folders
#' names(diffs)                     # the content of these files is different
#' str(diffs)                       # compact summary
#' diffs                            # show all lines that are different
#'
#' # Compare file that exists in both folders
#' diff(x, y, "DESCRIPTION")  # same as diffs$DESCRIPTION
#' }
#'
#' @aliases diff
#'
#' @export
#' @export diff.character

diff.character <- function(x, y, file=NULL, lines=FALSE, short=TRUE,
                           simple=TRUE, ...)
{
  if(dir.exists(x) && dir.exists(y))
  {
    if(is.null(file))
    {
      if(lines)
      {
        files <- intersect(dir(x), dir(y))
        files <- files[!(files %in% list.dirs(x, full.names=FALSE))]  # no dirs
        out <- mapply(diff.file, file.path(x, files), file.path(y, files),
                      short=short, simple=simple, SIMPLIFY=FALSE, ...)
        names(out) <- files
        if(simple)
          out <- out[!sapply(out, is.null)]
        out
      }
      else
      {
        diff.dir(x, y, short=short, simple=simple)
      }
    }
    else
    {
      diff.file(file.path(x, file), file.path(y, file), short=short,
                simple=simple, ...)
    }
  }
  else if(file.exists(x) && file.exists(y))
  {
    diff.file(x, y, short=short, simple=simple, ...)
  }
  else
  {
    if(!file.exists(x))
      stop("'", x, "' not found")
    if(!file.exists(y))
      stop("'", y, "'not found")
  }
}

diff.dir <- function(x, y, short=TRUE, simple=TRUE)
{
  ## 1  Read directories
  A <- dir(x)
  A <- A[!(A %in% list.dirs(x, full.names=FALSE))]  # no dirs
  B <- dir(y)
  B <- B[!(B %in% list.dirs(y, full.names=FALSE))]  # no dirs

  ## 2  Compare
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  out <- list(diffA, diffB)
  names(out) <- if(short) short.name(x, y) else c(x, y)

  ## 3  Replace character(0) with NULL
  if(simple)
  {
    out[sapply(out, length) == 0] <- NULL
    if(length(out) == 0)
      out <- NULL
  }
  out
}

diff.file <- function(x, y, short=TRUE, simple=TRUE, ...)
{
  ## 1  Read files
  A <- readLines(x, ...)
  B <- readLines(y, ...)

  ## 2  Compare
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  out <- list(diffA, diffB)
  names(out) <- if(short) short.name(x, y) else c(x, y)

  ## 3  Replace character(0) with NULL
  if(simple)
  {
    out[sapply(out, length) == 0] <- NULL
    if(length(out) == 0)
      out <- NULL
  }
  out
}

short.name <- function(A, B)
{
  ## 1  Convert \\ to /
  A <- gsub("\\\\", "/", A)
  B <- gsub("\\\\", "/", B)

  ## 2  Distinguish between three cases
  ## Case one: identical, nothing to do - only happens when user runs diff(x, x)
  ## Case two: basename is unique, use that
  ## Case three: basename is identical, chop off basename until it's unique
  if(A == B)
    c(A, B)
  else if(basename(A) != basename(B))
    c(basename(A), basename(B))  # x/y/A.txt and x/y/B.txt => A.txt and B.txt
  else
    short.name(dirname(A), dirname(B))  # x/A/y/n.txt and x/B/y/n.txt => A and B
}
