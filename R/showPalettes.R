#' Show Palettes
#'
#' Show system palettes.
#'
#' @param palette palette name, or \code{NULL} to show all palettes.
#'
#' @return List of palettes.
#'
#' @seealso
#' \code{\link{showColors}}.
#'
#' @importFrom graphics barplot par title
#' @importFrom grDevices palette.pals palette.colors
#'
#' @export

showPalettes <- function(palette=NULL, cex=1)
{
  barpal <- function(i)
  {
    col <- rev(pal[[i]])
    n <- length(pal[[i]])
    main <- paste0(names(pal)[i], "\n(", n, ")")
    barplot(rep(1, n), col=col, horiz=TRUE, axes=FALSE, border=NA)
    title(main=main, font.main=1, cex.main=cex)
  }

  if(is.null(palette))
  {
    par(mfrow=c(2, 8), plt=c(0.1, 0.9, 0.1, 0.7))
    on.exit(par(mfrow=c(1,1), plt=c(0.1286275, 0.9341176, 0.16, 0.8713725)))
    pal <- sapply(palette.pals(), palette.colors, n=NULL)
  }
  else
  {
    pal <- list(palette.colors(palette=palette))
    names(pal) <- palette
  }

  for(i in 1:length(pal))
    barpal(i)

  invisible(pal)
}
