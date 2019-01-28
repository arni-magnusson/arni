#' Package Dependencies
#'
#' Find package dependencies or reverse dependencies.
#'
#' @param packages package names.
#' @param recursive whether to include dependencies of dependencies.
#' @param reverse whether to find reverse dependencies instead.
#' @param base whether to include base packages.
#' @param installed whether to include installed packages.
#' @param available whether to include available packages.
#' @param sort whether to sort package dependencies.
#' @param \dots passed to \code{package_dependencies}.
#'
#' @return
#' Names of packages that are required by \code{package}.
#'
#' @seealso
#' \code{\link{package_dependencies}} is the underlying base function to find
#' package dependencies.
#'
#' \code{\link{installed.packages}}, \code{\link{available.packages}}.
#'
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages installed.packages
#'
#' @export

pdeps <- function(packages, recursive=TRUE, reverse=FALSE, base=FALSE,
                  installed=TRUE, available=TRUE, sort=FALSE, ...)
{
  ## Get all package dependencies
  pkgs <- package_dependencies(packages, recursive=recursive,
                               reverse=reverse, ...)

  ## Maybe exclude base/installed/available
  if(!base)
    pkgs <- lapply(pkgs, function(p)
      p[!(p %in% rownames(installed.packages(priority="high")))])
  if(!installed)
    pkgs <- lapply(pkgs, function(p) p[!(p %in% rownames(installed.packages()))])
  if(!available)
    pkgs <- lapply(pkgs, function(p) p[!(p %in% rownames(available.packages()))])

  ## Format output
  pkgs <- pkgs[sapply(pkgs,length) > 0]  # remove empty elements
  if(sort)
    pkgs <- lapply(pkgs, sort)

  pkgs
}
