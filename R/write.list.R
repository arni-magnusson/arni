#' Write List
#'
#' Write list to a text file.
#'
#' @param x a list.
#' @param file output filename.
#'
#' @importFrom utils capture.output
#'
#' @export

write.list <- function(x, file="")
{
  txt <- capture.output(print(x))
  txt <- trimws(txt)
  cat(txt, sep="\n", file=file)
}
