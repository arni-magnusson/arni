#' Require Package
#'
#' Install package if it is not already installed.
#'
#' @param package package name(s).
#'
#' @return Logical indicating whether the package is ready to be loaded.
#'
#' @export

Require <- function(package)
{
  if(length(package) > 1)
  {
    sapply(package, Require)
  }
  else
  {
    ## Check if package is already installed
    if(!(package %in% rownames(installed.packages())))
      install.packages(package)

    ## Indicate whether package is installed
    package %in% rownames(installed.packages())
  }
}
