#' Convert EPS to PNG.
#'
#' Convert EPS file to PNG format.
#'
#' @param epsfile EPS file to convert.
#' @param dpi dots per inch.
#' @param gray whether image should be grayscale.
#' @param ag antialias value for graphics, from 0 (sharp) and 4 (smooth).
#' @param at antialias value for text, from 0 (sharp) and 4 (smooth).
#'
#' @note
#' Requires shell scripts \code{2png} and \code{optipng}.
#'
#' @return
#' \code{NULL}, but alters EPS file and creates PDF file.
#'
#' @export

eps2png <- function(epsfile, dpi=300, gray=FALSE, ag=4, at=4)
{
  if(!file.exists(epsfile))
    stop(epsfile, " not found. Please verify filename.")

  png.args <- if(.Platform$OS.type=="windows")
                paste0("-ag=", ag, " -at=", at, " -dpi=", dpi,
                       if(gray) " -gray")
              else
                paste("-a", ag, "-b", at, "-d", dpi, if(gray) "-g")
  png.cmd <- paste("2png", png.args, epsfile)
  pngfile <- paste0(file_path_sans_ext(epsfile), ".png")
  opt.cmd <- paste("optipng", pngfile)

  system(png.cmd)
  system(opt.cmd)
}
