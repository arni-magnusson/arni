#' X Intercept
#'
#' Evaluate the x intercept and its SE in a simple LM.
#'
#' @param model a fitted lm or glm of the form \code{y~x}.
#' @param se whether to report the standard error.
#'
#' @note
#' The standard error is evaluated using delta method approximation.
#'
#' In a logistic glm(family=binomial), the x intercept corresponds to the LD50
#'   inflection point.
#'
#' @section Warning:
#' In reality, the uncertainty about the x intercept is asymmetric.
#'
#' The delta method SE approximation is inaccurate when the slope is close to
#' zero.
#'
#' Clearly, the x intercept SE is infinity when the slope is not significantly
#' different from zero.
#'
#' @return
#' List containing \code{est} (estimated x intercept) and \code{se} (standard
#' error), or if \code{se = FALSE} a simple number.
#'
#' @importFrom stats coef vcov
#'
#' @export

x0 <- function(model, se=TRUE)
{
  b <- coef(model)
  b0 <- b[[1]]
  b1 <- b[[2]]
  x0 <- -b0/b1

  v <- vcov(model)
  Varb0 <- v[1,1]
  Varb1 <- v[2,2]
  Covb0b1 <- v[1,2]

  Varx0 <- x0^2 * (Varb0/b0^2 + Varb1/b1^2 - 2*Covb0b1/(b0*b1))
  output <- if(se) list(est=x0, se=sqrt(Varx0)) else x0

  output
}
