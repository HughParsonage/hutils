#' Logical implies
#' @description Returns the result of \eqn{x\implies y}.
#' @param x,y Logical vectors of the same length.
#' @return \code{TRUE} along \code{y} unless \code{x} is \code{TRUE} and \code{y} is \code{FALSE}.
#' @export %implies% implies
#' 
#' @examples 
#' 
#' c(TRUE, TRUE, FALSE, FALSE) %implies%
#' c(TRUE, FALSE, TRUE, FALSE)
#' 
#' 


implies <- function(x, y) {
  lx <- length(x)
  ly <- length(y)
  stopifnot(lx == ly, is.logical(x), is.logical(y))
  out <- y
  out[!x] <- TRUE
  out
}

#' @rdname implies
`%implies%` <- function(x, y) {
  lx <- length(x)
  ly <- length(y)
  stopifnot(is.logical(x), is.logical(y))
  if (lx != ly) {
    dx <- deparse(substitute(x))
    dy <- deparse(substitute(y))
    stop(dx, " and ", dy, " had unequal lengths ",
         "(length(x) = ", lx, "; ",
         "(length(y) = ", ly, ").",
         "\n",
         "`x` and `y` must be the same length.")
  }
  out <- y
  out[!x] <- TRUE
  out
}
