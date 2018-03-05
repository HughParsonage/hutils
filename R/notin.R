#' Negation of in
#' @param x Values to be matched
#' @param y Values to be matched against.
#' @details If \code{y} is \code{NULL}, then \code{x} is \code{TRUE} for consistency with
#'  \code{\%in\%}. Note that the function uses \code{\link[fastmatch]{fmatch}} internally for 
#' performance on large \code{y}. Accordingly, \code{y} will be modified by adding
#' a \code{.match.hash} attribute and thus must not be used in packages where \code{y}
#' is a constant, or for things like names of \code{data.table}.
#' @export

`%notin%` <- function(x, y){
  if (is.null(y)) {
    rep_len(TRUE, length(x))
  } else {
    is.na(fmatch(x, y))
  }
}


