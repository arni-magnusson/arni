#' Cumulative Quantiles
#'
#' Calculate cumulative quantiles.
#'
#' @param z a numeric vector.
#' @param probs a vector of desired quantiles.
#'
#' @note
#' Implemented and embedded in \code{cumuplot} in the \pkg{coda} package.
#'
#' @return
#' Data frame containing cumulative quantiles, with samples in rows and
#' probabilities in columns.
#'
#' @export

cquantile <- function(z, probs=c(0.05, 0.25, 0.50, 0.75, 0.95))
{
  cquant <- matrix(0, nrow=length(z), length(probs))

  for(i in seq_along(z))
    cquant[i, ] <- quantile(z[1:i], probs=probs, names=FALSE)
  cquant <- as.data.frame(cquant)
  names(cquant) <- paste0(formatC(100*probs,format="fg",width=1,digits=7), "%")

  cquant
}
