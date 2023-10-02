#' Show Palettes
#'
#' Show system palettes.
#'
#' @param palette palette name, or \code{NULL} to show all palettes.
#' @param cex text size for main title.
#'
#' @return List of palettes.
#'
#' @seealso
#' \code{\link{showColors}}.
#'
#' @examples
#' showPalettes()
#' showPalettes("Tableau")
#'
#' f <- function() barplot(rep(1, 9), col=col)
#' opar <- par(mfrow=c(2, 3))
#' col <- palette.colors(9, "Set 1"); f()
#' col <- palette.colors(9, "Tableau"); f()
#' col <- palette.colors(9, "Classic"); f()
#' col <- palette.colors(9, "Paired"); f()
#' col <- palette.colors(9, "Alphabet"); f()
#' col <- palette.colors(9, "Polychrome"); f()
#' par(opar)
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
