#' Density Plot
#'
#' Draw vertical density plots by factor.
#'
#' @param \dots a list of values, typically from split(x,factor).
#' @param name whether factor names should be displayed on x axis.
#' @param quantiles a vector of quantiles where lines should be drawn.
#' @param plot.mean whether a thick line should be drawn at each mean.
#' @param main passed to \code{plot}.
#' @param xlab passed to \code{plot}.
#' @param ylab passed to \code{plot}.
#' @param ylim passed to \code{plot}.
#' @param las passed to \code{plot} and \code{axis}.
#'
#' @return
#' Vector containing subplot baselines on the x axis.
#'
#' @author Alistair Dunn, with minor changes by Arni Magnusson.
#'
#' @importFrom graphics axis lines
#' @importFrom stats density
#'
#' @export

dplot <- function(..., name=TRUE, quantiles=0.5, plot.mean=FALSE, main="",
                  xlab="", ylab="", ylim, las=1)
{
  ## Revised version to allow for vectors of length 1
  dlines <- function(y, offset, Q)
  {
    y <- y[!is.na(y)]
    if(min(y) == max(y))
    {
      ans <- list(x=min(y), y=1)
      y1 <- ans$x
      x1 <- ans$y / max(ans$y) * 0.8
      q.y1 <- quantile(y, probs=c(0,Q,1))
      q.x1 <- offset + x1
      q.x2 <- offset
      mean.y1 <- mean(y)
      mean.x1 <- offset + x1
      mean.x2 <- offset
    }
    else
    {
      ans <- density(y, n=100, from=min(y), to=max(y))
      y1 <- ans$x
      x1 <- ans$y / max(ans$y) * 0.8
      q.y1 <- quantile(y, probs=c(0,Q,1))
      q.x1 <- offset + approx(y1,x1,xout=q.y1)$y
      q.x2 <- offset
      mean.y1 <- mean(y)
      mean.x1 <- offset + approx(y1,x1,xout=mean.y1)$y
      mean.x2 <- offset
    }
    list(x1=offset+x1, y1=y1, q.x1=q.x1, q.y1=q.y1, q.x2=q.x2,
         mean.x1=mean.x1, mean.x2=mean.x2, mean.y1=mean.y1)
  }

  all.x <- list(...)
  nam <- character(0)
  if(is.list(all.x[[1]]))
  {
    all.x <- all.x[[1]]
    if(is.logical(name) && name)
      name <- names(...)
  }
  n <- length(all.x)
  centers <- seq.int(from=0,by=1,length.out=n) + 0.1
  ymax <- max(sapply(all.x,max,na.rm=TRUE), na.rm=TRUE)
  if(is.na(ymax))
    stop("Error: list of empty vectors")
  ymin <- min(sapply(all.x,min,na.rm=TRUE), na.rm=TRUE)
  xmax <- max(centers) + 0.95
  xmin <- 0
  if(!missing(ylim))
    plot(c(xmin,xmax), c(ymin,ymax), type="n", main=main, xlab=xlab, ylab=ylab,
         xaxt="n", las=las, ylim=ylim)
  else
    plot(c(xmin,xmax), c(ymin,ymax), type="n", main=main, xlab=xlab, ylab=ylab,
         xaxt="n", las=las)
  for(i in 1:n)
  {
    if(length(all.x[[i]][!is.na(all.x[[i]])]) > 0)
    {
      plot.values <- dlines(all.x[[i]], centers[i], quantiles)
      lines(plot.values$x1, plot.values$y1)
      segments(centers[i], min(plot.values$y1), centers[i], max(plot.values$y1))
      segments(plot.values$q.x1, plot.values$q.y1, plot.values$q.x2,
               plot.values$q.y1)
      if(plot.mean)
        segments(plot.values$mean.x1, plot.values$mean.y1, plot.values$mean.x2,
                 plot.values$mean.y1, lwd=3)
    }
  }
  if(is.logical(name) && name)
    axis(1, centers, sapply(substitute(list(...)),as.character)[2:(n+1)],
         las=las, adj=ifelse(las<=1,0.5,1))
  else
    axis(1, centers, name, las=las, adj=ifelse(las==1,0.5,1))

  invisible(centers)
}
