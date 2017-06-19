#' Standard Error
#'
#' Calculate the standard error of the mean.
#'
#' @param x a numeric vector.
#'
#' @return
#' Standard error as a number.
#'
#' @export

se <- function(x)
{
  s <- sd(x)
  n <- length(x)

  s / sqrt(n)
}
