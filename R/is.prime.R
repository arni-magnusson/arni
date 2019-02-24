#' Prime Numbers
#'
#' Test whether number is a prime number.
#'
#' @param n the number to check.
#' @param verbose whether to report the smallest divisor.
#'
#' @return Logical specifying whether number is a prime number.
#'
#' @seealso
#' \code{primes} in the \pkg{sfsmisc} package.
#'
#' @export

is.prime <- function(n, verbose=FALSE)
{
  if(length(n) > 1)
  {
    sapply(n, is.prime, verbose=verbose)
  }
  else
  {
    if(n < 2)
      return(FALSE)  # n<2 is not a prime number
    if(n == 2)
      return(TRUE)   # n=2 is a prime number
    if(n%%2 == 0)
    {
      if(verbose) cat(n, "is divisible by 2\n")  # n>2 and even is not prime
      return(FALSE)
    }

    div <- 3
    while(div <= sqrt(n))  # test odd numbers from 3 to sqrt(n)
    {
      if(n%%div == 0)
      {
        if(verbose) cat(n, " is divisible by ", div, "\n", sep="")
        return(FALSE)  # divisor found
      }
      div <- div + 2
    }
    ## No divisor found
    TRUE
  }
}
