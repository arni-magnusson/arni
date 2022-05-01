#' Require Package
#'
#' Install package if it is not already installed.
#'
#' @param package name of a package.
#'
#' @return Logical indicating whether the package is ready to be loaded.
#'
#' @export

Require <- function(package)
{
  ## Check if package is already installed
  if(!(package %in% rownames(installed.packages())))
    install.packages(package)

  ## Indicate whether package is installed
  package %in% rownames(installed.packages())
}
