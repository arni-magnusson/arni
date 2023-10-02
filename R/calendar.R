#' Calendar
#'
#' Draw circular calendar with days and months.
#'
#' @param filename EPS output file, or \code{NULL} to draw on screen.
#' @param year calendar year.
#' @param lwd line width.
#' @param radius vector of 4 numbers, setting distance from center to: (1) day
#'        labels, (2) notes boundary, (3) month boundary, (4) year boundary.
#' @param cex vector of 3 numbers, setting font size of: (1) day labels, (2)
#'        month labels, (3) year label.
#' @param col vector of 4 colors (recycled), setting bg color of: (1) day areas,
#'        (2) notes areas, (3) month areas, (4) year area.
#' @param \dots passed to \code{text}.
#'
#' @return Vector of radians (\code{cos} and \code{sin} give day coordinates).
#'
#' @importFrom graphics symbols segments text
#' @importFrom grDevices dev.off
#'
#' @export

calendar <- function(filename=NULL, year=2009, lwd=0.1,
                     radius=c(1.03,0.95,0.8,0.65), cex=c(0.3,0.8,1.6),
                     col="white", ...)
{
  ## 1  Get days
  first <- as.POSIXct(paste0(year,"-01-01"))
  last <- as.POSIXct(paste0(year,"-12-31"))
  days <- seq(first, last, by="day")   # POSIXct of length 365 or 366
  d <- as.integer(format(days, "%d"))  # 1, ..., 31
  b <- format(days, "%b")              # "Jan", ..., "Dec"
  n <- length(days)

  ## 2  Get coordinates
  L <- seq.int(0, 1-1/n, 1/n)        # left outer point of wedge
  M <- seq.int(0.5/n, 1-0.5/n, 1/n)  # mid outer point of wedge
  R <- seq.int(1/n, 1, 1/n)          # right outer point of wedge
  L2p <- -2*pi*L + pi/2
  M2p <- -2*pi*M + pi/2
  R2p <- -2*pi*R + pi/2

  ## 3  Vectorize graphical parameters
  radius <- rep(radius, length=4)
  cex <- rep(cex, length=3)
  col <- rep(col, length=4)

  ## 4  Draw
  if(!is.null(filename)) eps(filename)
  plot(NA, xlim=c(-1,1), ylim=c(-1,1), asp=1, ann=FALSE, axes=FALSE)
  ## Days
  symbols(0, 0, circles=1, inches=FALSE, add=TRUE, lwd=lwd, bg=col[1])
  segments(0, 0, cos(L2p), sin(L2p), lwd=lwd)
  text(radius[1]*cos(M2p), radius[1]*sin(M2p), d, xpd=TRUE, cex=cex[1], ...)
  ## Notes
  symbols(0, 0, circles=radius[2], inches=FALSE, add=TRUE, lwd=lwd, bg=col[2])
  segments(0, 0, cos(L2p[d==1]), sin(L2p[d==1]), lwd=lwd)
  ## Months
  symbols(0, 0, circles=radius[3], inches=FALSE, add=TRUE, lwd=lwd, bg=col[3])
  segments(0, 0, cos(L2p[d==1]), sin(L2p[d==1]), lwd=lwd)
  text(mean(radius[3:4])*cos(M2p[d==15]), mean(radius[3:4])*sin(M2p[d==15]),
       b[d==15], cex=cex[2], ...)
  ## Center
  symbols(0, 0, circles=radius[4], inches=FALSE, add=TRUE, lwd=lwd, bg=col[4])
  if(!is.null(filename)) dev.off()
  text(0, 0, year, cex=cex[3])

  output <- M2p

  invisible(output)
}
