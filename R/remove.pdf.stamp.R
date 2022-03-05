#' Remove PDF Stamp
#'
#' Replace PDF stamp like \samp{(Downloaded by [Arni Magnusson] ...)} with
#' spaces.
#'
#' @param file a PDF document.
#'
#' @return Overwrites original PDF file, after removing stamps.
#'
#' @note Similar to Emacs Lisp function \verb{remove-pdf-stamp}.
#'
#' @export

remove.pdf.stamp <- function(file)
{
  spaces <- function(n)
    paste(rep(" ",n), collapse="")
  blank <- function(n)
    paste0("(", spaces(n-4), ")Tj")

  txt <- readLines(file)
  stamp <- grepl("^\\(Downloaded by \\[", txt)
  n <- nchar(txt[stamp])
  txt[stamp] <- sapply(n, blank)

  writeLines(txt, file)
}
