#' Number to Binary
#'
#' Convert number to binary format.
#'
#' @param x is number.
#' @param width minimum width for output string.
#'
#' @return
#' Binary string.
#'
#' @export

num2bin <- function (x, width=NULL)
{
  if(length(x) > 1)
  {
    sapply(x, num2bin, width=width)
  }
  else
  {
    n <- ceiling(log(x+1, 2))  # number of bits needed, or 0 if x=0
    v <- integer(n)

    for(i in n:1)  # 0:1 is correct if x=0
    {
      v[i] <- x%%2
      x <- x %/% 2
    }

    output <- paste(v, collapse="")
    if(!is.null(width))
      output <- chartr(" ", "0", formatC(output,width=width))

    output
  }
}
