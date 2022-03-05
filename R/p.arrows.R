#' Arrows with Solid Heads
#'
#' Draw arrows with solid heads.
#'
#' @param x1 x start coordinates.
#' @param y1 y start coordinates.
#' @param x2 x end coordinates.
#' @param y2 y end coordinates.
#' @param size arrowhead size.
#' @param width arrowhead width.
#' @param fill arrowhead fill color.
#' @param border arrowhead border color.
#' @param \dots passed to \code{segments}.
#'
#' @return \code{NULL}, but arrowhead is drawn.
#'
#' @note Borrowed from package \code{sfsmisc}, adding \code{border} argument.
#'
#' @importFrom graphics polygon xyinch
#'
#' @export

p.arrows <- function(x1, y1, x2, y2, size=1, width=(sqrt(5)-1)/4/cin, fill=2,
                     border=NULL, ...)
{
  cin <- size * par("cin")[2]
  uin <- if(is.R()) 1/xyinch() else par("uin")
  segments(x1, y1, x2, y2, ...)

  x <- sqrt(seq(0, cin^2, length=floor(35*cin)+2))
  delta <- 0.005 / 2.54
  x.arr <- c(-x, -rev(x))
  wx2 <- width * x^2
  y.arr <- c(-wx2-delta, rev(wx2)+delta)
  deg.arr <- c(atan2(y.arr,x.arr), NA)
  r.arr <- c(sqrt(x.arr^2+y.arr^2), NA)
  theta <- atan2((y2-y1)*uin[2], (x2-x1)*uin[1])
  lx <- length(x1)
  Rep <- rep.int(length(deg.arr), lx)
  x2 <- rep.int(x2, Rep)
  y2 <- rep.int(y2, Rep)
  theta <- rep.int(theta, Rep) + rep.int(deg.arr, lx)
  r.arr <- rep.int(r.arr, lx)

  polygon(x2+r.arr*cos(theta)/uin[1], y2+r.arr*sin(theta)/uin[2], col=fill,
          border=border)
}
