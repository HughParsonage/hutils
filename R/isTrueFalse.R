#' Logical assertions
#' 
#' @param x An object whose values are to be checked.
#' @return For \code{isTrueFalse}, \code{TRUE} if and only if \code{x} is \code{TRUE} or \code{FALSE} identically (perhaps with attributes). 
#' 

isTrueFalse <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

areTrueFalse <- function(...) {
  vapply(list(...), isTrueFalse, logical(1L))
}




