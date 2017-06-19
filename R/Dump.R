#' Dump Objects
#'
#' Dump objects into one or separate files.
#'
#' @param list a vector of object names.
#' @param file a file or directory where objects should be dumped.
#' @param envir where to search for objects.
#' @param pager a program to view output file, or \code{FALSE} if output file
#'        should not be opened
#'
#' @details
#' If \code{file} does not exist and has no file extension, then a new directory
#' is created.
#'
#' If \code{file} is a directory, it should not end with a \code{/}, unless it's
#' a drive name.
#'
#' Specify \code{envir} to prevent dumping the wrong object when synonymous
#' objects exist.
#'
#' @return
#' \code{NULL}, but objects are dumped to file(s) and possibly opened in pager.
#'
#' @importFrom tools file_ext
#'
#' @export

Dump <- function(list=if(length(ls(1))>0) ls(1)[sapply(ls(1),
                   function(x)is.function(get(x,1)))] else NULL,
                 file=paste0(dirname(tempdir()),"/temp.R"),
                 envir=.GlobalEnv, pager=getOption("pager"))
{
  if(!file.exists(file) && file_ext(file)=="")  # create new directory
    dir.create(file)

  if(dir.exists(file))  # existing drive or directory
  {
    sapply(list, function(x) dump(x, paste0(file,"/",x,".R"), envir=envir))
  }
  else  # existing file, or a valid file to write
  {
    ## Create empty file in case list is NULL (possibly overwriting an old one)
    file.create(file)
    dump(list, file, envir=envir)
    if(is.character(pager)) file.show(file)
  }

  invisible(NULL)
}
