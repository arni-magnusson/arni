#' Yule's K
#'
#' Calculate Yule's characteristic \eqn{K}.
#'
#' @param x a vector of labels, e.g. people names or words in a book.
#'
#' @note
#' Large value of \eqn{K} indicates lower diversity.
#'
#' \eqn{K} is proportional to the probability of any two randomly selected items
#' to be identical.
#'
#' @references
#' Yule, G.U. 1944. The statistical study of literary vocabulary. Cambridge:
#' Cambridge University Press.
#'
#' McElduff, F., P. Mateos, A. Wade, and M.C. Borja. 2008. What's in a name? The
#'   frequency and geographic distributions of UK surnames. Significance
#'   5:189-192
#'
#' @seealso
#' \code{\link{shannon}}.
#'
#' @return
#' \eqn{K} as a single value
#'
#' @export

yule <- function(x)
{
  N <- length(x)
  V <- table(table(x))
  r <- as.integer(names(V))

  K <- 1e4 * 1/N^2 * (sum(r^2*V) - N)

  K
}
