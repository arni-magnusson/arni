#' HSV Colors
#'
#' Plot HSV colors as a wheel with full saturation and value.
#'
#' @param n the number of hue values.
#' @param col an optional set of colors, to show some other palette.
#' @param labels a scale to use as labels: \code{"n"}:none, \code{1}:0-1,
#'        \code{255}:0-255, \code{360}:0-360, \code{"f"}:0-f.
#' @param bg the background color.
#'
#' @return \code{NULL}, but a plot is drawn.
#'
#' @seealso
#' \code{\link{hclDemo}}, \code{\link{rgbDemo}}.
#'
#' @importFrom graphics par pie text
#' @importFrom grDevices hsv
#'
#' @export

hsvDemo <- function(n=400, col=hsv(seq(0,1-1/n,1/n)), labels="n", bg="black")
{
  opar <- par(bg=bg); on.exit(par(opar))
  labels <- as.character(labels)
  labels <- match.arg(labels, c("n","1","255","360","f"))

  pie(rep(1,n), labels=NA, edges=1e4, radius=1, clockwise=TRUE, col=col,
      border=FALSE)

  if(labels != "n")
  {
    lab <- switch(labels, "1"=seq(0,0.9,0.1), "255"=seq(0,255,16),
                  "360"=seq(0,330,30), "f"=c(0:9,letters[1:6]))
    nlab <- length(lab)
    h <- seq.int(0, 1-1/nlab, 1/nlab)
    a <- 2*pi*h + pi/2  # angle from north in radians
    text(-1.1*cos(a), 1.1*sin(a), lab, xpd=TRUE, col="grey", cex=0.8)
  }

  invisible(NULL)
}
