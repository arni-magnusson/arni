#' Bind PDF Book
#'
#' Bind book from many PDF files.
#'
#' @param directory where the PDF files are.
#' @param merge whether to merge the chapters - otherwise return \code{cmd}.
#' @param redistill whether to redistill \file{out.pdf} (pdftk output) to
#'        \file{_out.pdf} (Ghostscript output).
#'
#' @return
#' Shell command string used to merge PDF files with \code{pdftk.}
#'
#' @note
#' The PDF files should be named something like \file{0.pdf 1.pdf 27.pdf 42.pdf
#'   ... 397.pdf 413a.pdf 413b.pdf}.
#'
#' Before this function is called, unnecessary pages can be peeled out of
#' \file{0.pdf}, and the last file often needs splitting, e.g. \file{413.pdf}
#' into \file{413a.pdf} and \file{413b.pdf}.
#'
#' Three files are written to directory: \file{_} (blank page of right size),
#' \file{out.pdf} (book merged with pdftk), and \file{_out.pdf} (\file{out.pdf}
#' redistilled with Ghostscript).
#'
#' After this function is called, one should check \file{out.pdf} to see whether
#' pages are correctly numbered, and \file{out_.pdf} to see whether the
#' Ghostscript-redistilled document is smaller while retaining intact images and
#' no red borders around links
#'
#' Warning: The final Ghostscript redistillation is launched with
#' \code{wait = FALSE}, so the best way to see if it is finished is to check if
#' any Ghostscript processes are active.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom graphics frame
#' @importFrom grDevices pdf dev.off
#'
#' @export

book <- function(directory, merge=TRUE, redistill=TRUE)
{
  getInfo <- function(file)
  {
    cmd <- paste("pdfinfo", file)
    info <- system(cmd, intern=TRUE)
    info
  }
  getPages <- function(text)
  {
    line <- grep("Pages:", text, value=TRUE)
    pp <- as.integer(round(as.numeric(strsplit(line, " +")[[1]][2])))
    pp
  }
  getWidth <- function(text)
  {
    line <- grep("Page size:", text, value=TRUE)
    w <- as.integer(round(as.numeric(strsplit(line, " +")[[1]][3])))
    w
  }
  getHeight <- function(text)
  {
    line <- grep("Page size:", text, value=TRUE)
    h <- as.integer(round(as.numeric(strsplit(line, " +")[[1]][5])))
    h
  }

  owd <- setwd(directory); on.exit(setwd(owd))

  ## 1  Sort filenames
  chapters <- dir(directory, pattern="\\.pdf$")
  chapters <- grep("out", chapters, value=TRUE, invert=TRUE)
  trails <- gsub("[0-9]", "", file_path_sans_ext(chapters))  # the a in 439a.pdf
  if(any(nzchar(trails)))
    trails[nzchar(trails)] <- paste0(".", hex2num(trails[nzchar(trails)]))
  decimal <- as.numeric(paste0(gsub("[a-z]", "", file_path_sans_ext(chapters)),
                               trails))
  chapters <- chapters[order(decimal)]

  ## 2  Get info
  info <- lapply(chapters, getInfo)
  pages <- sapply(info, getPages)
  width <- sapply(info, getWidth)
  height <- sapply(info, getHeight)

  ## 3  Add blank pages
  ## Base size on file 2 (probably chapter 1 in book)
  pdf(paste(directory,"_",sep="/"), width[2]/72, height[2]/72)
  frame()
  dev.off()
  add <- as.logical(c(0, pages[-c(1,length(pages))]%%2, 0))
  chapters[add] <- paste(chapters[add], "_")

  ## 4  Merge
  cmd.merge <- paste("pdftk", paste(chapters,collapse=" "),
                     "output out.pdf dont_ask")
  if(!merge)
    return(cmd.merge)
  system(cmd.merge)

  ## 5  Redistill in Ghostscript
  if(!redistill)
    return(cmd.merge)
  cmd.redistill <- "2pdf out.pdf"
  system(cmd.redistill, wait=FALSE)

  ## 6  Report
  report <- data.frame(chapters, pages, add, width, height)

  list(cmd=cmd.merge, report=report)
}
