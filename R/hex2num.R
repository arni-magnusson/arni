#' Hexadecimal to Number
#'
#' Convert hexadecimal string to number.
#'
#' @param x a hexadecimal string, like \code{"FF"}.
#'
#' @return
#' Decimal number.
#'
#' @export

hex2num <- function(x)
{
  as.numeric(paste0("0x", x))  # 12x faster than as.hexmode(x)
}
