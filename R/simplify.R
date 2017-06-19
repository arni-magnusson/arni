#' Simplify Object
#'
#' Coerce object to the simplest possible storage mode.
#'
#' @param x a list, data frame, matrix, or vector.
#'
#' @note
#' \verb{factor -> character -> numeric -> integer}
#'
#' If user knows that \code{x} is a factor coercible to numeric/integer, then
#' \code{as.numeric(levels(x))[x]} is faster.
#'
#' Numbers outside the two billion range are not coerced to integers:
#' \code{c(-2147483647, 2147483647)}.
#'
#' Faster than \code{type.convert(as.character(x))}.
#'
#' @return
#' Object of same dimensions as \code{x}, using simplest possible storage modes.
#'
#' @export

simplify <- function(x)
{
  ## Silently check whether as.numeric("x") returns NA or numeric
  owarn <- options(warn=-1); on.exit(options(owarn))

  ## List or data frame
  if(is.list(x))
  {
    for(i in seq_len(length(x)))
      x[[i]] <- simplify(x[[i]])
  }
  ## Matrix
  else if(is.matrix(x))
  {
    if(is.character(x) && sum(is.na(as.numeric(x)))==sum(is.na(x)))
      mode(x) <- "numeric"
    if(is.numeric(x))
    {
      y <- as.integer(x)
      if(sum(is.na(x))==sum(is.na(y)) && all(x==y,na.rm=TRUE))
        mode(x) <- "integer"
    }
  }
  ## Vector
  else
  {
    if(is.factor(x))
      x <- as.character(x)
    ## The commented code was simpler,
    ## but performing as.numeric() twice was too expensive
    ## if(is.character(x) && sum(is.na(as.numeric(x)))==sum(is.na(x)))
    ##   x <- as.numeric(x)
    if(is.character(x))
    {
      y <- as.numeric(x)
      if(sum(is.na(y)) == sum(is.na(x)))
        x <- y
    }
    if(is.numeric(x))
    {
      y <- as.integer(x)
      if(sum(is.na(x))==sum(is.na(y)) && all(x==y,na.rm=TRUE))
        x <- y
    }
  }
  x
}
