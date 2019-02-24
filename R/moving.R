#' Moving Average
#'
#' Calculate moving average.
#'
#' @param x a numeric vector.
#' @param k how many values are evaluated in moving average.
#' @param ends how the ends should be treated.
#'
#' @details
#' Alternative \code{end} treatments are:
#' \tabular{ll}{
#'   \code{NA} \tab use \code{NA} values\cr
#'   \code{FALSE} truncate, so output is shorter than \code{x}\cr
#'   \code{TRUE} evaluate moving average on ends, even if the neighbors are all
#'   on one side
#' }
#'
#' @return Vector of same length as \code{x}, containing weighted averages.
#'
#' @note
#' If \code{k} is an even number, a weighted average is used with half weights
#' on each end.
#'
#' This function is mainly useful if the ends have flat trends, e.g. a diagonal
#' line will have bent ends...
#'
#' @seealso \code{\link[caTools]{runmean}}.
#'
#' @importFrom stats weighted.mean
#'
#' @export

moving <- function(x, k=5, ends=NA)
{
  if(length(x) == 1)
    return(x)

  original.names <- names(x)
  x <- as.vector(as.matrix(x))  # x is now a vector
  rows <- 2*floor(k/2)+1        # number of rows in working matrix
  cols <- length(x)             # number of cols in working matrix

  middle <- (rows+1) / 2
  shifted <- trunc(k/2)              # number of shifted vectors on each side
  m <- matrix(nrow=rows, ncol=cols)  # will contain original and shifted vectors
  m[middle,] <- x                    # insert original vector into middle

  if(shifted > 0)
    for(s in 1:shifted)
    {
      ## Insert left-shifted vectors above middle
      m[middle-s,] <- c(x[-(1:s)], rep(NA,s))
      ## Insert right-shifted vectors below middle
      m[middle+s,] <- c(rep(NA,s), x[-((cols-s+1):cols)])
    }

  wts <- rep(1, rows)
  if(k%%2 == 0)            # if k is an even number...
    wts[c(1,rows)] <- 0.5  # ...assign half weights on each end

  output <- apply(m, 2, weighted.mean, w=wts, na.rm=TRUE)
  names(output) <- original.names

  if(is.na(ends))
    output[c(1:shifted, (cols-shifted+1):cols)] <- NA
  else if(is.logical(ends) && !ends)
    output <- output[-c(1:shifted, (cols-shifted+1):cols)]

  output
}
