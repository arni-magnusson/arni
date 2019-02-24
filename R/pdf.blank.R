#' Blank PDF
#'
#' Create blank PDF page.
#'
#' @param file the filename to create.
#' @param width the page width in inches.
#' @param height the page height in inches.
#'
#' @return Invisible string that was written to PDF file.
#'
#' @note Exact size, unlike the \code{pdf} device.
#'
#' @export

pdf.blank <- function(file, width=8.5, height=11)
{
  w <- width * 72                                  # 612
  h <- height * 72                                 # 792
  wh <- formatC(c(w,h), format="f", digits=3)      # "612.000" "792.000"
  numbers <- paste(c("0","0",wh), collapse=" ")    # "0 0 612.000 792.000"
  MediaBox <- paste0("/MediaBox [", numbers, "]")  # "/MediaBox [0 0 612.000 ...

  output <- paste(
      "%PDF-1.4",
      "1 0 obj <</Type /Catalog /Outlines 2 0 R /Pages 3 0 R>> endobj",
      "2 0 obj <</Type /Outlines /Count 0>> endobj",
      "3 0 obj <</Type /Pages /Kids [4 0 R] /Count 1>> endobj",
      "4 0 obj <</Type /Page /Parent 3 0 R",
      MediaBox,
      "/Contents null /Resources <</ProcSet null /Font <</F1 null>>>>>> endobj",
      "xref 0 5",
      "0000000000 65535 f ",
      "0000000009 00000 n ",
      "0000000072 00000 n ",
      "0000000116 00000 n ",
      "0000000171 00000 n ",
      "trailer <</Size 5 /Root 1 0 R>>",
      "startxref",
      "311",
      "%%EOF", "", sep="\n")  # "" for final endline

  con <- file(file, open="wb")
  cat(output, file=con)
  close(con)

  invisible(output)
}
