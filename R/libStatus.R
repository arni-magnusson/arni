#' Library Status
#'
#' Summarize status of libraries.
#'
#' @param lib.loc a string vector indicating which libraries to analyze.
#' @param repositories a string vector indicating which repositories to analyze.
#' @param full whether to return a data frame with complete details about all
#'        installed packages.
#'
#' @return
#' List of two data frames,
#' \item{count}{package count}
#' \item{details}{packages that are not "ok"}
#' and a third data frame
#' \item{full}{all installed packages}
#' if \code{full = TRUE}.
#'
#' @importFrom stats addmargins aggregate xtabs
#' @importFrom utils contrib.url packageStatus
#'
#' @export

libStatus <- function(lib.loc=.libPaths(),
                      repositories=contrib.url(getOption("repos")), full=FALSE)
{
  inst <- packageStatus(lib.loc=lib.loc, repositories=repositories)$inst

  count <- aggregate(Package~Status+LibPath, data=inst, length)
  count <- xtabs(Package~Status+LibPath, data=count)
  count <- addmargins(count)
  count <- as.data.frame(unclass(count))

  details <- inst[inst$Status!="ok", c("Status","LibPath")]
  details <- details[order(details$Status, details$LibPath,
                           row.names(details)),]

  if(full)
    output <- list(count=count, details=details, full=inst)
  else
    output <- list(count=count, details=details)

  output
}
