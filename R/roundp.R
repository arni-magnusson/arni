#' Round Proportions
#'
#' Round proportions so they sum to one (or allocate seats from election
#' results).
#'
#' @param x a vector containing proportions (or votes from an election).
#' @param digits a number of decimals to use when rounding proportions, max 6.
#' @param seats the number of seats to allocate from election results.
#' @param method \code{"DH"}, \code{"MSL"}, or \code{"SL"} (default), indicating
#'        the algorithm to use.
#' @param labels an optional vector of labels.
#'
#' @note
#' This function should be called either with a \code{digits} argument, or with
#' a \code{seats} argument, not both.
#'
#' The algorithms were designed to allocate seats from elections:
#' \tabular{ll}{
#'   \code{"DH"}  \tab (D'Hondt) favors big parties - used in most of Europe and
#'                     elsewhere\cr
#'   \code{"MSL"} \tab (modified Sainte-Lague) favors big parties slightly -
#'                     used in Norway and Sweden\cr
#'   \code{"SL"}  \tab (Sainte-Lague) does not favor big or small parties - used
#'                     in New Zealand, Bosnia, and Latvia
#' }
#'
#' @return
#' Numeric vector of same length as \code{x}, with rounded proportions whose sum
#' is 1 (or integers whose sum is \code{seats}).
#'
#' @export

roundp <- function(x, digits=NULL, seats=NULL, method="SL", labels=names(x))
{
  method <- match.arg(toupper(method), c("DH","MSL","SL"))

  if(is.null(digits) && is.null(seats) || !is.null(digits) && !is.null(seats))
    stop("Please pass a value as 'digits' or 'seats', not both")
  if(!is.null(digits))
  {
    if(digits<0 || digits>6)
      stop("Please pass a positive value (0-6) as 'digits'")
    n <- as.integer(10^digits)
  }
  else
    n <- seats

  party <- seq_along(x)
  series <- switch(method,
                   DH  = 1 + 1  *(seq_len(n)-1),  # 1, 2,   3,   ..., n
                   MSL = 1 + 1.4*(seq_len(n)-1),  # 1, 2.4, 3.8, ..., 1.4n-0.4
                   SL  = 1 + 2  *(seq_len(n)-1))  # 1, 3,   5,   ..., 2n-1

  output <- data.frame(party=rep(party,each=n),
                       score=as.numeric(sapply(x, function(votes)
                         votes/series)))
  output <- factor(output$party)[order(-output$score)][seq_len(n)]
  output <- as.integer(table(output))

  if(!is.null(digits))
    output <- output / n
  names(output) <- labels

  output
}
