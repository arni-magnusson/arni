#' Transform Table to Cohort Format
#'
#' Transform \code{Year | Age | Value} table into cohort format, and possibly
#' create a plot.
#'
#' @param x a \code{Year | Age | Value} table (three column data frame or
#'        crosstabbed data frame, matrix, or table).
#' @param xtab whether the output should be returned as a wide crosstab table
#'        (otherwise long data frame).
#' @param plot whether a cohort plot should be created.
#' @param log a string specifying which axes should be log-transformed:
#'        \code{""}, \code{"x"}, \code{"y"}, \code{"xy"}, or \code{TRUE -> "y"}.
#' @param base the log base used if \code{log} is \code{TRUE}.
#' @param type passed to \code{matplot}.
#' @param lty passed to \code{matplot}.
#' @param xlab passed to \code{matplot}.
#' @param ylab passed to \code{matplot}.
#' @param \dots passed to \code{matplot}.
#'
#' @return
#' Crosstab table in cohort format, of class \code{c("xtabs","table")}.
#'
#' @note
#' If \code{x} is a three column data frame, column 1 must contain the year,
#' column 2 the age, and column 3 the value. If \code{x} is crosstabbed, rows
#' must be years and columns must be age.
#'
#' Warning: Zero values are treated as \code{NA}.
#'
#' @importFrom graphics matplot
#' @importFrom stats xtabs
#'
#' @export

cohorts <- function(x, xtab=TRUE, plot=TRUE, log="", base=exp(1), type="l",
                    lty=1, xlab="Age", ylab="Value", ...)
{
  if(ncol(x) > 3)
    x <- unxtab(x)  # x is now a three column data frame

  Year <- as.integer(x[,1])
  Age <- as.integer(x[,2])
  Cohort <- Year - Age
  Value <- x[,3]

  cohort.frame <- data.frame(Cohort, Year, Age, Value)
  names(cohort.frame) <- c("Cohort", names(x))
  cohort.xtab <- xtabs(Value~Age+Cohort)  # vectors outside cohort.frame
  cohort.xtab[cohort.xtab==0] <- NA
  names(dimnames(cohort.xtab)) <- names(x)[1:2]

  x.vector <- as.numeric(rownames(cohort.xtab))
  y.matrix <- cohort.xtab

  if(is.character(log))
  {
    if(grepl("x",log))
      x.vector <- log(x.vector, base)
    if(grepl("y",log))
      y.matrix <- log(y.matrix, base)
  }
  else if(is.logical(log))
  {
    if(log)
      y.matrix <- log(y.matrix, base)
  }

  if(xtab)
    output <- t(cohort.xtab)
  else
    output <- cohort.frame

  if(plot)
    matplot(x.vector, y.matrix, type=type, lty=lty, xlab=xlab, ylab=ylab, ...)

  output
}
