#' Compare Objects
#'
#' Compare two objects as sets.
#'
#' @param A object to compare.
#' @param B object to compare.
#' @param zoom whether to show only data frame columns whose values differ.
#' @param useSource whether source code format and comments matter, otherwise R
#'        standard format is used.
#' @param external whether to compare using an external viewer.
#' @param viewer an external program to use if \code{external=TRUE}.
#' @param \dots passed to \code{compare.*} methods.
#'
#' @return
#' List with 4 elements:
#' \item{both}{items found in both objects}
#' \item{A}{items found only in A}
#' \item{B}{items found only in B}
#' \item{same.data}{whether objects contain the same set of items}
#'
#' @note
#' Set comparison ignores duplicates and order: \code{c(1,1,2,3)} and
#' \code{c(3,2,1)} are similar. Similarly, when comparing two functions, the
#' following are the same set:
#'
#' \preformatted{
#' f <- function(x)    g <- function(x)
#' \{                   \{
#'   y <- 1              y <- 3
#'   y <- 1              y <- 2
#'   y <- 2              y <- 1
#'   y <- 3            \}
#' \}}
#'
#' Data frames are compared line by line, and must have the same dimensions.
#'
#' @export

compare <- function(A, B, ...)
{
  UseMethod("compare")
}

#' @rdname compare
#' @export

compare.data.frame <- function(A, B, zoom=TRUE, ...)
{
  if(any(dim(A)!=dim(B)))
    stop("Data frames must have same dimensions to be compared.")

  diffs <- A!=B
  rdiff <- rowSums(diffs,na.rm=TRUE) > 0
  cshow <- if(zoom) colSums(diffs,na.rm=TRUE)>0 else rep(TRUE,ncol(A))
  output <- list(A[!rdiff,], A[rdiff,cshow,drop=FALSE],
                 B[rdiff,cshow,drop=FALSE], sum(diffs,na.rm=TRUE)==0)
  names(output) <- c("both", Aname=deparse(substitute(A)),
                     Bname=deparse(substitute(B)), "same.data")

  output
}

#' @rdname compare
#' @export

compare.default <- function(A, B, ...)
{
  both  <- intersect(A, B)
  Aonly <- A[!(A %in% B)]  # faster than setdiff(A,B)
  Bonly <- B[!(B %in% A)]
  same.set <- length(Aonly)==0 && length(Bonly)==0

  output <- list(both, Aonly, Bonly, same.set)
  ## Use [1] to handle on-the-fly c(...) which could be very long
  names(output) <- c("both", deparse(substitute(A)), deparse(substitute(B)),
                     "same.set")

  output
}

#' @rdname compare
#' @export

compare.function <- function(A, B, useSource=TRUE, external=FALSE,
                             viewer=if(.Platform$OS.type=="windows") "winmerge"
                                    else "kompare", ...)
{
  ## 1  Coerce to functions and extract names
  if(!is.function(A))
    A <- get(A)
  if(!is.function(B))
    B <- get(B)
  Aname <- deparse(substitute(A))
  Bname <- deparse(substitute(B))

  ## 2  Compare functions internally and maybe externally
  useSource <- useSource && "source" %in% names(attributes(A)) &&
      "source" %in% names(attributes(B))  # if both exist
  Alines <- if(useSource) attr(A,"source") else deparse(A)
  Blines <- if(useSource) attr(B,"source") else deparse(B)
  output <- compare.default(Alines, Blines)
  noquote.matrix <- function(x)
    structure(x, dim=c(length(x),1), dimnames=list(rep("",length(x)),""),
              class="noquote")
  output[1:3] <- lapply(output[1:3], noquote.matrix)
  names(output) <- c("both", Aname, Bname, "same.set")
  if(external)
  {
    Afile <- paste0(dirname(tempdir()), "/", Aname, ".R")
    Bfile <- paste0(dirname(tempdir()), "/", Bname, ".R")
    control <- c("keepInteger", "keepNA", if(useSource) "useSource" else NULL)
    ## dput() rather than dump() to exclude function name
    dput(A, Afile, control=control); on.exit(unlink(Afile))
    dput(B, Bfile, control=control); on.exit(unlink(Bfile), add=TRUE)
    cmd <- paste(viewer, Afile, Bfile)
    system(cmd)
  }

  ## 3  Warn if same.set=TRUE but function lines are different
  if(output$same.set && !identical(Alines, Blines))
  {
    shorter <- min(length(Alines), length(Blines))
    width <- max(nchar(c(Aname, Bname)))
    diff1 <- which(Alines[seq_len(shorter)] != Blines[seq_len(shorter)])[1]
    warning("Although `", Aname, "` and `", Bname,
            "` contain the same set of lines, they're still different.\n  ",
            "Lines are duplicated and/or in different order. ",
            "The first difference is:\n  ",
            format(Aname,width=width,justify="right"), " [", diff1, "]  ",
            Alines[diff1], "\n  ",
            format(Bname,width=width,justify="right"), " [", diff1, "]  ",
            Blines[diff1])
  }
  output
}
