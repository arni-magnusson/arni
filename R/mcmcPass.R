#' MCMC Tests
#'
#' Run diagnostic tests on an MCMC trace.
#'
#' @param x an MCMC trace.
#' @param return.type output format.
#'
#' @details
#' Three \code{return.type} formats are supported:
#' \tabular{ll}{
#'   \code{"logical"} \tab a single logical value, indicating whether all tests
#'   were passed\cr
#'   \code{"list"} \tab a detailed list of all test results\cr
#'   \code{"string"} \tab a string indicating which tests failed
#' }
#'
#' @return Test results in the format specified by \code{return.type}.
#'
#' @note
#' The test criteria are:
#' \enumerate{
#'   \item Autocorrelation <= 0.1
#'   \item Geweke Z <= 1.96
#'   \item Heidelberger-Welch p >= 0.05
#' }
#'
#' Users can apply other criteria by requesting \code{return.type = "list"}.
#'
#' @importFrom coda geweke.diag heidel.diag
#' @importFrom stats acf cor var
#'
#' @export

mcmcPass <- function(x, return.type="logical")
{
  ## 1  Enforce argument value
  return.type <- match.arg(return.type, c("logical", "list", "string"))

  ## 2  Run tests
  auto.rho <- if(var(x)>0) acf(x, plot=FALSE, lag.max=1)$acf[2] else NA_real_
  auto.rho <- if(var(x)>0) cor(x[-length(x)], x[-1]) else NA_real_
  geweke.z <- if(var(x)>0) unname(geweke.diag(x)$z) else NA_real_
  heidel.p <- unname(heidel.diag(x)[,"pvalue"])
  pass.a <- abs(auto.rho) <= 0.1
  pass.g <- abs(geweke.z) <= 1.96
  pass.h <- heidel.p >= 0.05

  ## 3  Format results
  pass <- all(pass.a, pass.g, pass.h)
  detail <- list(auto.rho=auto.rho, geweke.z=geweke.z, heidel.p=heidel.p,
                 pass=pass)
  string <- if(is.na(pass))
              NA_character_
            else
              paste0(if(!pass.a) "a" else " ",
                     if(!pass.g) "g" else " ",
                     if(!pass.h) "h" else " ")

  ## 4  Return requested format
  output <- switch(return.type, logical=pass, list=detail, string=string)

  output
}
