% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unicode.R
\name{unicode}
\alias{unicode}
\title{Unicode}
\usage{
unicode(x = 0:255, reverse = FALSE)
}
\arguments{
\item{x}{a code point as decimal or hexadecimal number (or a character if
\code{reverse = TRUE}).}

\item{reverse}{whether to get a point point from a character.}
}
\value{
String vector, or integer vector if \code{reverse = TRUE}.
}
\description{
Get character from the Unicode code chart (or get code point of a character
if \code{reverse = TRUE}).
}
\note{
Hexadecimal number can be passed as \code{0xfe} or \code{"fe"}, but not
\code{"0xfe"}.
}
