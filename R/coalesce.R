#' Find first non-missing element
#' @description Lightweight version of \code{dplyr::coalesce}, with all the vices and virtues that come from such an
#' approach. 
#' Very similar logic (and timings to \code{dplyr::coalesce}), though no ability to use quosures etc.
#' One exception is that if \code{x} does not contain any missing values, it is returned immediately,
#' and ignores \code{...}. For example, \code{dplyr::coalesce(1:2, 1:3)} is an error, but 
#' \code{hutils::coalesce(1:2, 1:3)} is not.
#' 
#' @param x A vector
#' @param ... Successive vectors whose values will replace the corresponding values in \code{x} if the value is 
#' (still) missing.
#' @return \code{x} with missing values replaced by the first non-missing corresponding elements in \code{...}.
#' That is, if \code{... = A, B, C} and \code{x[i]} is missing, then \code{x[i]} is replaced by
#' \code{A[i]}. If \code{x[i]} is still missing (i.e. \code{A[i]} was itself \code{NA}), then it
#' is replaced by \code{B[i]}, \code{C[i]} until it is no longer missing or the list has been exhausted.
#' @examples 
#' coalesce(c(1, NA, NA, 4), c(1, 2, NA, NA), c(3, 4, 5, NA))
#' @export coalesce
#' @source Original source code but obviously inspired by \code{dplyr::coalesce}.

# 
# 
# The MIT License (MIT)
# =====================
#   
#   Copyright (C) 2013-2015 RStudio and others.
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the ``Software''), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
#   
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

coalesce <- function(x, ...) {
  if (!anyNA(x) || missing(..1)) {
    return(x)
  } else {
    values <- list(...)
    
    lx <- length(x)
    lengths <- c(lx, vapply(values, length, FUN.VALUE = 0L))
    lengthsn1 <- lengths != 1L
    if (any(lengthsn1 & lengths != lx)) {
      which_wrong_length <- which(lengthsn1 & lengths != lx)
      stop("Argument ", which_wrong_length[1], " had length ", lengths[which_wrong_length[1]], ". ", 
           "The only permissible vector lengths in ... are 1 or the length of `x` (", lx, ").")
    }
    
    lv <- length(values)
    
    i <- 1L
    while (i == 1L ||  # already checked the conditions
           i <= lv && anyNA(x)) {
      vi <- values[[i]]
      nax <- is.na(x)
      if (lengthsn1[i + 1L]) {
        x[nax] <- vi[nax]
      } else {
        x[nax] <- vi
      }
      i <- i + 1L
    }
    x
  }
}
