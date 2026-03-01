#' Heart
#'
#' Oscillating function.
#'
#' @param x value(s) between -2.718 and 2.718.
#'
#' @return Value(s) corresponding to f(x).
#'
#' @note
#' \deqn{f(x)=\sin(\pi^3x)\times\frac{\sqrt{e^2-x^2}}{2}+\sqrt{|x|}}{
#'       f(x) = sin(pi^3 * x) * sqrt(exp(2)-x^2)/2 + sqrt(abs(x))}
#'
#' @examples
#' x <- seq(-exp(1), exp(1), length=1000)
#' plot(x, heart(x), type="l", col=2, lwd=2)
#'
#' @export

heart <- function(x)
{
  sin(pi^3 * x) * sqrt(exp(2)-x^2)/2 + sqrt(abs(x))
}
