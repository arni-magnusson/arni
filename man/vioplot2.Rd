% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vioplot2.R
\name{vioplot2}
\alias{vioplot2}
\title{Violin Plot}
\usage{
vioplot2(x, ..., orientation = "vertical", bw = "nrd0", names = NULL,
  pars = NULL)
}
\arguments{
\item{x}{a vector or data frame.}

\item{\dots}{passed to polygon.}

\item{orientation}{[not supported].}

\item{bw}{bandwidth algorithm, passed to \code{density}.}

\item{names}{a vector of x-axis tick labels.}

\item{pars}{[not supported].}
}
\value{
Invisible vector of midpoints, useful for adding to the plot.
}
\description{
Draw violin plot, a wavy boxplot made of symmetric density plots.
}
\note{
Ignores many standard graphical arguments, so axis labels should be added
afterwards.
}
\seealso{
\pkg{vioplot} package that doesn't support lists.
}
\author{
John Verzani, with minor changes by Arni Magnusson.
}
