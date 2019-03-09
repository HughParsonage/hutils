#' Swap assignment
#' 
#' @name swap
#' @param x,value Objects whose values are to be reassigned by swapping.
#' @return \code{NULL} invisibly. Called for its side-effect: the values
#' of \code{x} and \code{value} are swapped. So
#' \preformatted{x \%<->\% value} 
#' is equivalent to
#' \preformatted{temp <- x
#' x <- value
#' value <- temp
#' rm(temp)}
#' 
#' @export


'%<->%' <- function(x, value) {
  if (exists("***temp***", envir = parent.frame())) {
    stop("Unable to use <->")
  }
  eval.parent(substitute(assign("***temp***", value = force(x))))
  eval.parent(substitute(x <- value))
  eval.parent(substitute(value <- `***temp***`))
  rm(list = "***temp***", envir = parent.frame())
}
