#' Bouncing Ball
#'
#' Demonstrate bouncing ball.
#'
#' @param n number of time steps to simulate.
#' @param init initial x and y coordinates (vector of length 2).
#' @param vel initial x and y velocity (vector of length 2).
#' @param xlim x-axis limits (vector of length 2).
#' @param ylim y-axis limits (vector of length 2).
#' @param pch point symbol.
#' @param col point color.
#' @param cex point size.
#'
#' @return \code{NULL}, but \code{n} plots are drawn.
#'
#' @importFrom graphics plot
#'
#' @export

bounce <- function(n=5000, init=c(3,1), vel=c(0.03,0.04), xlim=c(0,10),
                   ylim=c(0,10), pch=16, col="blue", cex=1.5)
{
  x <- init[1]
  y <- init[2]
  vx <- vel[1]
  vy <- vel[2]

  for(i in 1:n)
  {
    if(x+vx<=xlim[1] || x+vx>=xlim[2])
      vx <- -vx
    if(y+vy<=ylim[1] || y+vy>=ylim[2])
      vy <- -vy
    x <- x + vx
    y <- y + vy
    plot(x, y, xlim=xlim, ylim=ylim, ann=FALSE, xaxt="n", yaxt="n", pch=pch,
         col=col, cex=cex)
  }
}
