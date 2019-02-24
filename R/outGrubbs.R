#' Outlier Test
#'
#' Detect outliers in normal-distributed sample, using Grubbs test.
#'
#' @param x a numeric vector.
#' @param output the required output, see Value below.
#' @param alpha the significance level for a two-way test.
#' @param log whether outlier detection should be performed on \code{log(x)}.
#'
#' @return
#' Vector like \code{x} with outliers removed, or if \code{output="outliers"}
#' the outlier values, or if \code{output="which"} a vector indicating which
#' elements are outliers.
#'
#' @note
#' Grubbs (1969) cited in Sokal and Rohlf (1995, p. 407), recommending sample
#' size > 25.
#'
#' @importFrom stats qt
#'
#' @export

outGrubbs <- function(x, output=c("stripped","outliers","which"), alpha=0.05,
                      log=FALSE)
{
  crit <- function(x, alpha)
  {
    n <- length(x)
    t <- qt(alpha/(2*n), df=n-2, lower.tail=FALSE)
    value <- (n-1)/sqrt(n) * sqrt((t^2)/(n-2+t^2))
    if(alpha==0) value <- 0  # alpha=0 means no datapoint is an outlier
    value
  }

  output <- match.arg(output)
  if(length(x)<=25) warning("Use Dixon's test when sample size is 25 or less")
  if(log) x <- log(x)

  ok <- seq_along(x)  # elements that have not been branded as outliers
  while(max(abs(x[ok]-mean(x[ok]))/sd(x[ok])) > crit(x[ok],alpha))
    ok <- ok[-which.max(abs(x[ok]-mean(x[ok]))/sd(x[ok]))]
  if(log) x <- exp(x)
  output <- switch(output, stripped=x[ok], outliers=x[-ok],
                   which=seq_along(x)[-ok])

  output
}
