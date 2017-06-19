#' Shannon Index
#'
#' Calculate Shannon index of species diversity.
#'
#' @param x a frequency vector, one value per species.
#' @param bounded whether to bound the index between 0 and 1.
#'
#' @seealso
#' \code{\link{yule}}.
#'
#' @return
#' Species diversity as a number.
#'
#' @export

shannon <- function(x, bounded=TRUE)
{
  x <- x[x!=0]
  if(length(x) < 2)
    return(0)

  p <- x/sum(x)
  S <- length(x)

  Hprime <- -sum(p*log(p))

  if(bounded)
    output <- Hprime / log(S)
  else
    output <- Hprime

  output
}
