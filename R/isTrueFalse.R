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

check_TF <- function (x) {
  if (is.logical(x) && length(x) == 1L) {
    if (anyNA(x)) {
      xc <- deparse(substitute(x))
      stop("`", xc, " = NA` but must be TRUE or FALSE. ", 
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      return(NULL)
    }
  } else {
    xc <- deparse(substitute(x))
    if (length(x) != 1L) {
      stop("`", xc, "` had length ", length(x), " but must be length-one. ", 
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      stop("`", xc, "` was type ", typeof(x), " but must be logical. ", 
           "Change `", xc, "` to be TRUE or FALSE.")
    }
  }
}



