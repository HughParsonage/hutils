#' Swap assignment
#' @name swap
#' @param x,y Objects whose values are to be reassigned by swapping.
#' @return \code{NULL} invisibly. Called for its side-effect: the values
#' of \code{x} and \code{y} are swapped. So
#' \preformatted{x \%<->\% y} 
#' is equivalent to
#' \preformatted{temp <- x
#' x <- y
#' y <- temp
#' rm(temp)}
#' 
#' @export


`%<->%` <- function(x, y) {
  if (exists("***temp***", envir = parent.frame())) {
    stop("Unable to use <->")
  }
  eval.parent(substitute(assign("***temp***", value = force(x))))
  eval.parent(substitute(x <- y))
  eval.parent(substitute(y <- `***temp***`))
  rm(list = "***temp***", envir = parent.frame())
}
