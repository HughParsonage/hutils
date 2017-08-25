#' Negation of in
#' @param x Values to be matched
#' @param y Values to be matched against.
#' @details If \code{y} is \code{NULL}, then \code{x} is \code{TRUE} for consistency with \code{\%in\%}.
#' @export 

`%notin%` <- function(x, y){
  if (is.null(y)) {
    rep_len(TRUE, length(y))
  } else {
    is.na(fmatch(x, y))
  }
}
