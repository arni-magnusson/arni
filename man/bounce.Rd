% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bounce.R
\name{bounce}
\alias{bounce}
\title{Bouncing Ball}
\usage{
bounce(n = 5000, init = c(3, 1), vel = c(0.03, 0.04), xlim = c(0, 10),
  ylim = c(0, 10), xaxs = "i", yaxs = "i", pch = 16, col = "blue",
  cex = 1.5)
}
\arguments{
\item{n}{number of time steps to simulate.}

\item{init}{initial x and y coordinates (vector of length 2).}

\item{vel}{initial x and y velocity (vector of length 2).}

\item{xlim}{x-axis limits (vector of length 2).}

\item{ylim}{y-axis limits (vector of length 2).}

\item{xaxs}{x-axis style.}

\item{yaxs}{y-axis style.}

\item{pch}{point symbol.}

\item{col}{point color.}

\item{cex}{point size.}
}
\value{
\code{NULL}, but \code{n} plots are drawn.
}
\description{
Demonstrate bouncing ball.
}
