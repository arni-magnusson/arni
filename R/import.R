#' Import Data
#'
#' Read data from clipboard.
#'
#' @param header passed to \code{read.table}.
#' @param \dots passed to \code{read.table}.
#'
#' @note
#' Data are read from the clipboard using \code{read.table}, and then converted
#' to a vector if the data are in a single row or single column.
#'
#' @return
#' Vector or data frame from clipboard.
#'
#' @importFrom utils read.table
#'
#' @export

import <- function(header=FALSE, ...)
{
  ## Suppress warning about incomplete final line
  x <- suppressWarnings(read.table("clipboard", header=header, ...))

  if(nrow(x)==1 || ncol(x)==1)
    x <- unname(unlist(x))

  x
}
