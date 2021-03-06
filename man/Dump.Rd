% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dump.R
\name{Dump}
\alias{Dump}
\title{Dump Objects}
\usage{
Dump(list = if (length(ls(1)) > 0) ls(1)[sapply(ls(1), function(x)
  is.function(get(x, 1)))] else NULL, file = paste0(dirname(tempdir()),
  "/temp.R"), envir = .GlobalEnv, pager = getOption("pager"))
}
\arguments{
\item{list}{a vector of object names.}

\item{file}{a file or directory where objects should be dumped.}

\item{envir}{where to search for objects.}

\item{pager}{a program to view output file, or \code{FALSE} if output file
should not be opened}
}
\value{
\code{NULL}, but objects are dumped to file(s) and possibly opened in pager.
}
\description{
Dump objects into one or separate files.
}
\details{
If \code{file} does not exist and has no file extension, then a new directory
is created.

If \code{file} is a directory, it should not end with a \code{/}, unless it's
a drive name.

Specify \code{envir} to prevent dumping the wrong object when synonymous
objects exist.
}
