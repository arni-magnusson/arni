#' Show Colors
#'
#' Show all X11 colors.
#'
#' @param cex point size.
#'
#' @return \code{NULL}, but colors are shown in a plot.
#'
#' @note
#' Based on script presented by Kyle Ambert and Steven Bedrick in BMI 507/607,
#' Lecture 10 (30 Oct 2009).
#'
#' @importFrom graphics plot.new rect
#' @importFrom grDevices col2rgb colors
#'
#' @export

showColors <- function(cex=0.6)
{
  opar <- par(mar=c(0,0,0,0), cex.lab=1.4, cex.axis=1.4, cex.main=1.4,
              cex.sub=1.4); on.exit(par(opar))
  colorNames <- colors()  # don't sort: gray7 between gray69 and gray70
  plot.new()
  for(j in 1:11)
  {
    for(i in 1:60)
    {
      kolor <- colorNames[(j-1)*60+i]
      rect((j-1)/11, 1-(i-1)/60, j/11, 1-i/60, col=kolor)
      text((j-0.5)/11, 1-(i-0.5)/60, kolor,
           col=ifelse(mean(col2rgb(kolor))<120,"white","black"), cex=cex)
    }
  }
  invisible(NULL)
}
