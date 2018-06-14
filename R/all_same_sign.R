#' Determine whether a vector is all of the same sign
#' @description Present since \code{hutils 1.2.0}.
#' @param x A numeric vector. 
#' @return \code{TRUE} if all elements of \code{x} have the same sign. Zero is a separate sign from positive and negative. All vectors of length-1 or length-0 return \code{TRUE}, even if \code{x} = \code{NA}, (since although the value is unknown, it must have a unique sign), and non-numeric \code{x}.
#' 
#' @examples
#' all_same_sign(1:10)
#' all_same_sign(1:10 - 1)
#' all_same_sign(0)
#' all_same_sign(NA)
#' all_same_sign("surprise?")
#' all_same_sign(c(0, 0.1 + 0.2 - 0.3))
#' 
#' if (requireNamespace("microbenchmark", quietly = TRUE)) {
#'   microbenchmark::microbenchmark(length(unique(sign(1:1e5), nmax = 3)) == 1L, 
#'                                  all_same_sign(1:1e5))
#' }
#' 
#' @export

all_same_sign <- function(x) {
  OR(length(x) <= 1L,
     {
       x1 <- x[1L]
       if (x1 == 0) {
         AND(min(x) == 0,
             max(x) == 0)
       } else if (x1 > 0) {
         min(x) > 0
       } else {
         max(x) < 0
       }
     })
}

