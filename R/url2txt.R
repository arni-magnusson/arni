#' URL to Text
#'
#' Decode URL to text.
#'
#' @param x a percent-encoded URL string, like
#'        \code{"http\%3A\%2F\%2Fwww.hafro.is\%2F\%7Earnima"}.
#'
#' @return Plain text, like \code{"http://www.hafro.is/~arnima"}.
#'
#' @note
#' Percent-encoding is defined in RFC 3986
#' (\url{https://tools.ietf.org/html/rfc3986#section-2.1}).
#'
#' @export

url2txt <- function(x)
{
  from <- c("%20", "%21", "%22", "%23", "%24", "%25", "%26", "%27", "%28",
            "%29", "%2A", "%2B", "%2C", "%2D", "%2E", "%2F", "%3A", "%3B",
            "%3C", "%3D", "%3E", "%3F", "%40", "%5B", "%5C", "%5D", "%5E",
            "%5F", "%60", "%7B", "%7C", "%7D", "%7E")
  to <- c(" ", "!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-",
          ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "\\\\", "]", "^",
          "_", "`", "{", "|", "}", "~")

  for(i in 1:length(from))
    x <- gsub(from[i], to[i], x)

  x
}
