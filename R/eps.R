#' EPS Device
#'
#' Open postscript device for EPS output.
#'
#' @param file output filename.
#' @param width device width in inches.
#' @param height device height in inches.
#' @param pointsize text size in pts.
#' @param horizontal whether to use landscape layout.
#' @param onefile whether to produce page output, otherwise EPS output.
#' @param paper the kind of paper to print on.
#' @param \dots passed to \code{postscript}.
#'
#' @note
#' EPS options \code{horizontal}, \code{onefile}, and \code{paper} are explained
#' on the \code{postscript} help page.
#'
#' @return
#' \code{NULL}, but a graphics device is opened.
#'
#' @importFrom grDevices postscript
#'
#' @export

eps <- function(file="plot.eps", width=6, height=4, pointsize=10,
                horizontal=FALSE, onefile=FALSE, paper="special", ...)
{
  postscript(file=file, width=width, height=height, pointsize=pointsize,
             horizontal=horizontal, onefile=onefile, paper=paper, ...)
}
