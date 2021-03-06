% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmxy2.R
\name{lmxy2}
\alias{lmxy2}
\title{Multiple LM}
\usage{
lmxy2(X, y)
}
\arguments{
\item{X}{a matrix containing predictor values and 1 in first column.}

\item{y}{a vector (or single-column matrix) containing response values.}
}
\value{
Vector containing the coefficients.
}
\description{
Fit multiple LM fast, only returning the coefficients.
}
\seealso{
\code{\link{lmxy1}}.
}
