#' Distill EPS to PDF
#'
#' Tighten bounding box of EPS file and distill to PDF.
#'
#' @param epsfile EPS file to tighten and distill.
#' @param margin number of points between ink and the edge of the page, or
#'        \code{NULL} to skip tightening.
#'
#' @note
#' Old epsfile is overwritten.
#'
#' Similar EPS files can become misaligned when tightened, depending on whether
#' text has descenders.
#'
#' Requires shell script \code{2pdf} and Ghostscript.
#'
#' @return
#' \code{NULL}, but alters EPS file and creates PDF file.
#'
#' @export

eps2pdf <- function(epsfile, margin=3)
{
  if(!file.exists(epsfile))
    stop(epsfile, " not found. Please verify filename.")

  if(!is.null(margin))
  {
    stdout <- tempfile(); on.exit(unlink(stdout))            # stdout -> garbage
    stderr <- tempfile(); on.exit(unlink(stderr), add=TRUE)  # stderr -> bbox
    system(paste("gs -dBATCH -dEPSCrop -dNOPAUSE -sDEVICE=bbox", epsfile,
                 "1>", stdout, "2>", stderr))
    tight <- readLines(stderr)
    numbers <- substring(tight[substring(tight,1,14)=="%%BoundingBox:"], 16)
    ## E.g. 8 8 70 70
    numbers <- paste(as.numeric(unlist(strsplit(numbers," ")))+
                         c(-margin,-margin,+margin,+margin),collapse=" ")
    ## E.g. 5 5 73 73
    master <- readLines(epsfile)
    master[substring(master,1,14)=="%%BoundingBox:"] <- paste("%%BoundingBox:",
                                                              numbers)
    write(master, epsfile)
  }
  system(paste("2pdf",epsfile))

  invisible(NULL)
}
