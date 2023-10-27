#' Optimal X
#'
#' Evaluate the optimal x and its SE in a quadratic LM.
#'
#' @param model a fitted lm or glm of the form \code{y~x+I(x^2)}.
#' @param se whether to report the standard error.
#'
#' @return
#' List containing \code{est} (estimated optimal x) and \code{se} (standard
#' error), or if \code{se = FALSE} a simple number.
#'
#' @note
#' The standard error is evaluated using delta method approximation.
#'
#' The optimal x is the peak or bottom of the quadratic curve. See p. 54 in
#' Magnusson (2002).
#'
#' @references
#' Magnusson, A. (2002).
#' \emph{Survival rates of coho (Oncorhynchus kisutch) and chinook salmon (O.
#' tshawytscha) released from hatcheries on the U.S. and Canadian Pacific coast
#' 1972--1998, with respect to climate and habitat effects.}
#' M.S. thesis, University of Washington.
#'
#' @importFrom stats coef vcov
#'
#' @export

xopt <- function(model, se=TRUE)
{
  b <- coef(model)
  b1 <- b[[2]]
  b2 <- b[[3]]
  xopt <- -b1/(2*b2)

  v <- vcov(model)
  Varb1 <- v[2,2]
  Varb2 <- v[3,3]
  Covb1b2 <- v[2,3]

  Varxopt <- xopt^2 * (Varb1/b1^2 + Varb2/b2^2 - 2*Covb1b2/(b1*b2))
  output <- if(se) list(est=xopt, se=sqrt(Varxopt)) else xopt

  output
}
