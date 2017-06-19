#' FizzBuzz
#'
#' Solutions to FizzBuzz test. \code{fizz1} assigns values to vector subsets,
#' while \code{fizz2} assigns values to vector elements one by one.
#'
#' @return
#' String vector containing numbers, \code{Fizz}, \code{Buzz}, and
#' \code{FizzBuzz}.
#'
#' @note
#' The task is to print the numbers from 1 to 100, with the following
#' exceptions:
#' \itemize{
#' \item for multiples of three print \samp{Fizz} instead of the number
#' \item for multiples of five print \samp{Buzz}
#' \item for multiples of both three and five print \samp{FizzBuzz}
#' }
#'
#' @export

fizz1 <- function()
{
  x <- 1:100
  y <- as.character(x)

  y[x%%3==0] <- "Fizz"
  y[x%%5==0] <- "Buzz"
  y[x%%3==0 & x%%5==0] <- "FizzBuzz"

  y
}

#' @rdname fizz1
#'
#' @export

fizz2 <- function()
{
  y <- character(100)

  for(i in 1:100)
  {
    y[i] <-
      if(i%%3==0 && i%%5==0) "FizzBuzz"
      else if(i%%3==0) "Fizz"
      else if(i%%5==0) "Buzz"
      else i
  }

  y
}
