#' RGB Colors
#'
#' Plot RGB colors as a triangle.
#'
#' @param n the number of levels for each R/G/B component.
#' @param cex the size of plot symbols.
#' @param bg the background color.
#'
#' @return \code{NULL}, but a plot is drawn.
#'
#' @seealso
#' \code{\link{hclDemo}}, \code{\link{hsvDemo}}.
#'
#' @export

rgbDemo <- function(n=40, cex=1, bg="black")
{
  opar <- par(bg=bg); on.exit(par(opar))

  ## Exponential to fill triangle, skip black
  m <- expand.grid(R=(0:(n-1))^1.6, G=(0:(n-1))^1.6, B=(0:(n-1))^1.6)[-1,]
  prop <- m / rowSums(m)        # each row sums to one, for triangle mapping
  colors <- m / apply(m,1,max)  # each row has highest value one

  x <- 1 - prop$B - prop$R/2
  y <- prop$R * sin(pi/3)

  plot(x, y, ann=FALSE, axes=FALSE, pch=17, cex=cex, col=rgb(colors))

  invisible(NULL)
}
