#' Violin Plot
#'
#' Draw violin plot, a wavy boxplot made of symmetric density plots.
#'
#' @param x a vector or data frame.
#' @param \dots passed to polygon.
#' @param orientation [not supported].
#' @param bw bandwidth algorithm, passed to \code{density}.
#' @param names a vector of x-axis tick labels.
#' @param pars [not supported].
#'
#' @return
#' Invisible vector of midpoints, useful for adding to the plot.
#'
#' @note
#' Ignores many standard graphical arguments, so axis labels should be added
#' afterwards.
#'
#' @author
#' John Verzani, with minor changes by Arni Magnusson.
#'
#' @seealso
#' \pkg{vioplot} package that doesn't support lists.
#'
#' @importFrom graphics box plot.window
#' @importFrom grDevices rainbow
#'
#' @export

vioplot2 <- function(x, ..., orientation="vertical", bw="nrd0", names=NULL,
                     pars=NULL)
{
  vlnplt <- function(x, y, center, add=TRUE, orientation="horizontal",
                     bgcolor=NA, bordercolor="red", ...)
  {
    ## Double up first
    x <- c(x, rev(x))
    y <- c(y, -rev(y))
    y <- y + center
    if(orientation == "vertical")  # switch x and y
    {
      tmp <- x
      x <- y
      y <- tmp
    }
    if(add)
      polygon(x, y, ...)
  }

  args <- list(x, ...)
  namedargs <- if(!is.null(attributes(args)$names))
                 attributes(args)$names!="" else rep(FALSE,length=length(args))
  pars <- c(args[namedargs], pars)
  groups <- if(is.list(x)) x else args[!namedargs]
  if(0 == (n<-length(groups)))
    stop("invalid first argument")
  if(length(class(groups)))
    groups <- unclass(groups)
  if(!missing(names))
    attr(groups,"names") <- names
  else
  {
    if(is.null(attr(groups,"names")))
      attr(groups,"names") <- 1:n
    names <- attr(groups, "names")
  }

  ## Grab the global parameters, n-number of plots, N[i] number in each sample,
  ## work on the group by group level
  xvals <- matrix(0, nrow=512, ncol=n)
  yvals <- matrix(0, nrow=512, ncol=n)
  center <- 1:n  # where are they centered
  for(i in 1:n)
  {
    tmp.dens <- density(groups[[i]], bw=bw)
    xvals[,i] <- tmp.dens$x
    yvals.needtoscale <- tmp.dens$y
    ## Scale so largest size is less than 1/2
    yvals.scaled <- 7/16*yvals.needtoscale / max(yvals.needtoscale)
    yvals[,i] <- yvals.scaled
  }

  ## Now plot, need to first make the plot range, depending on orientation
  if(orientation == "vertical")
  {
    xrange <- c(1/2, n+1/2)  # each gets 1 unit centered on integers
    yrange <- range(xvals)
  }
  else  # horizontal
  {
    xrange <- range(xvals)
    yrange <- c(min(yvals), max(yvals))
  }

  plot.new()
  plot.window(xlim=xrange, ylim=yrange)
  for(i in 1:n)
    vlnplt(xvals[,i],yvals[,i],center[i], bordercolor=rainbow(i),
           bgcolor=rainbow(n-i), orientation=orientation, ...)
  axis(1, at=1:n, labels=names)
  axis(2)
  box()
  invisible(center)
}
