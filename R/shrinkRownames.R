#' Shrink Row Names
#'
#' Convert row names to integers.
#'
#' @param x a data frame.
#' @param force whether to overwrite current row names with integers 1, 2, 3,
#'        \dots, n.
#'
#' @return Same data frame, possibly with integer row names.
#'
#' @note
#' The default behavior is to convert row names only if they are strings
#' representing integers: \code{"1"}, \code{"2"}.
#'
#' @export

shrinkRownames <- function(x, force=FALSE)
{
  if(force)
  {
    row.names(x) <- seq_len(nrow(x))            # just overwrite with 1, 2, ...
  }
  else
  {
    r <- row.names(x)                           # check if "101", "102", ...
    ok <- suppressWarnings(identical(r, as.character(as.integer(r))))
    if(ok)
      row.names(x) <- as.integer(row.names(x))  # maybe convert to 101, 102, ...
  }

  x
}
