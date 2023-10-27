#' Yule's K
#'
#' Calculate Yule's characteristic \eqn{K}.
#'
#' @param x a vector of labels, e.g. people names or words in a book.
#'
#' @return
#' \eqn{K} as a single value.
#'
#' @note
#' Large value of \eqn{K} indicates lower diversity.
#'
#' \eqn{K} is proportional to the probability of any two randomly selected items
#' to be identical.
#'
#' @references
#' Yule, G.U. (1944).
#' \emph{The statistical study of literary vocabulary.}
#' Cambridge University Press, Cambridge.
#'
#' McElduff, F., Mateos, P., Wade, A., and Borja, M.C. (2008).
#' What's in a name? The frequency and geographic distributions of UK surnames.
#' \emph{Significance}, \bold{5}, 189--192.
#' \doi{10.1111/j.1740-9713.2008.00332.x}
#'
#' @seealso
#' \code{\link{shannon}}.
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
