#' Plot Correlation
#'
#' Plot correlation matrix as colored ellipses.
#'
#' @param corr a correlation matrix.
#' @param pow a power coefficient to choose colors for weak and strong
#'        correlations.
#' @param type which part of the matrix should be plotted: \code{"full"},
#'        \code{"lower"}, or \code{"upper"}.
#' @param order.fun is a function to reorder variables w.r.t. absolute
#'        correlations: e.g. \code{mean}, \code{median}, or \code{max}.
#' @param col is either \code{"bw"}, \code{"col"}, or a vector/matrix of colors.
#' @param \dots passed to \code{plotcorr}.
#'
#' @return Invisible correlation matrix, reordered if \code{order.fun} was used.
#'
#' @note Thin wrapper to pass colors to \code{plotcorr}.
#'
#' @importFrom ellipse plotcorr
#' @importFrom grDevices colorRamp gray rgb
#'
#' @export

plotcorr.col <- function(corr, pow=4, type="lower", order.fun=NULL,
                        col="col", ...)
{
  if(!is.null(order.fun))
  {
    diag(corr) <- NA
    ## Put large (neg or pos) correlations first
    ord <- order(apply(abs(corr),1,order.fun,na.rm=TRUE), decreasing=TRUE)
    diag(corr) <- 1
    corr <- corr[ord,ord]
  }

  if(col == "bw")
    col <- gray(1-abs(zapsmall(corr))^pow)
  if(col == "col")
    col <- rgb(colorRamp(c("red","white","blue"))
    ((sign(corr)*abs(zapsmall(corr))^pow+1)/2), maxColorValue=255)

  plotcorr(corr, type=type, col=col, ...)

  invisible(corr)
}
