#' GitHub User
#'
#' Look up email address of GitHub user.
#'
#' @param user GitHub username.
#'
#' @return Vector of strings containing \code{@} sign.
#'
#' @export

github <- function(user)
{
  url <- paste0("https://api.github.com/users/", user, "/events/public")
  x <- suppressWarnings(readLines(url))
  x <- unlist(strsplit(x, "\""))
  x <- grep("@", x, value=TRUE)
  x
}
