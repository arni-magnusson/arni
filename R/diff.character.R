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
#' @param ignore patterns (regular expressions) to exclude from the output.
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
#' This function uses \code{setdiff} for the comparison, so line order, line
#' numbers, and repeated lines are ignored.
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
#' y <- system.file("DESCRIPTION", package="stats")
#' diff(x, y)
#'
#' # Filter out noise
#' diff(x, y, ignore=c("Package:", "Title:", "Description:", "Built:"))
#'
#' # Compare filenames in two folders
#' A <- system.file(package="base")
#' B <- system.file(package="stats")
#' diff(A, B)               # these filenames are different
#' diff(A, B, ignore="^C")  # exclude entries starting with C
#'
#' # Compare content of files that exist in both folders
#' diff(A, B, lines=TRUE)           # the INDEX files are very different
#' diff(A, B, lines=TRUE, n=20)     # demonstrate passing n to readLines
#' diffs <- diff(A, B, lines=TRUE)  # store comparison as list
#' names(diffs)                     # these files are different
#' str(diffs, vec.len=1)            # first difference in each file
#'
#' # Alternative format
#' diff(A, B, ignore="^C")                             # short format
#' diff(A, B, ignore="^C", short=FALSE, simple=FALSE)  # long format
#'
#' # Compare one file that exists in both folders
#' diff(x, y, "DESCRIPTION")  # same as diffs$DESCRIPTION
#' }
#'
#' @aliases diff
#'
#' @export
#' @export diff.character

diff.character <- function(x, y, file=NULL, ignore=NULL, lines=FALSE,
                           short=TRUE, simple=TRUE, ...)
{
  ## 1  Calculate A and B entries, containing filenames or lines of text
  if(dir.exists(x) && dir.exists(y))
  {
    if(is.null(file))
    {
      if(lines)
      {
        files <- intersect(dir(x), dir(y))
        files <- files[!(files %in% list.dirs(x, full.names=FALSE))]  # -subdirs
        out <- list()
        for(f in files)
        {
          out[[f]] <- diff.character(file.path(x, f), file.path(y, f),
                                     ignore=ignore, lines=FALSE, short=short,
                                     simple=simple, ...)
        }
        names(out) <- files
        if(simple)
          out <- out[!sapply(out, is.null)]
        return(out)
      }
      else
      {
        A <- dir(x)
        A <- A[!(A %in% list.dirs(x, full.names=FALSE))]  # -subdirs
        B <- dir(y)
        B <- B[!(B %in% list.dirs(y, full.names=FALSE))]  # -subdirs
      }
    }
    else
    {
      A <- readLines(file.path(x, file), ...)
      B <- readLines(file.path(y, file), ...)
    }
  }
  else if(file.exists(x) && file.exists(y))
  {
    A <- readLines(x, ...)
    B <- readLines(y, ...)
  }
  else
  {
    if(!file.exists(x))
      stop("'", x, "' not found")
    if(!file.exists(y))
      stop("'", y, "'not found")
  }

  ## 2  Compare
  diffA <- setdiff(A, B)
  diffB <- setdiff(B, A)
  for(i in seq_along(ignore))
  {
    diffA <- grep(ignore[i], diffA, invert=TRUE, value=TRUE)
    diffB <- grep(ignore[i], diffB, invert=TRUE, value=TRUE)
  }
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
  ## case one: identical, nothing to do - only happens when user runs diff(x, x)
  ## case two: basename is unique, use that
  ## case three: basename is identical, chop off basename until it's unique
  if(A == B)
    c(A, B)
  else if(basename(A) != basename(B))
    c(basename(A), basename(B))  # x/y/A.txt and x/y/B.txt => A.txt and B.txt
  else
    short.name(dirname(A), dirname(B))  # x/A/y/n.txt and x/B/y/n.txt => A and B
}
