#' Prohibit vector recycling
#' @name prohibit_vector_recycling
#' @description Tests (harshly) whether the vectors can be recycled safely.
#' 
#' @param ... A list of vectors
#' @return An error message if the vectors are of different length (unless the alternative length is 1). 
#' The functions differ in their return values on success: \code{prohibit_vector_recycling.MAXLENGTH} 
#' returns the maximum of the lengths whereas \code{prohibit_vector_recyling} returns \code{NULL}. 
#' (Both functions return their values invisibly.)
#' @examples 
#' \dontrun{
#' # Returns nothing because they are of the same length
#' prohibit_vector_recycling(c(2, 2), c(2, 2))
#' # Returns nothing also, because the only different length is 1
#' prohibit_vector_recycling(c(2, 2), 1)
#' # Returns an error:
#' prohibit_vector_recycling(c(2, 2), 1, c(3, 3, 3))
#' }
#' @export

prohibit_vector_recycling <- function(...) {
  lengths <- lengths(list(...))
  max.length <- max(lengths)
  if (any(lengths != 1L & lengths != max.length)) {
    i <- which.max(lengths != 1L & lengths != max.length)
    j <- which.max(lengths)
    dots <- eval(substitute(alist(...)))
    first_wrong_arg <- as.character(dots[i])
    max_wrong_arg <- as.character(dots[j])
    stop("`", first_wrong_arg, "` had length ", lengths[i],
         ", but the length of `", max_wrong_arg, "` is ", max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  }
}

#' @rdname prohibit_vector_recycling
#' @export
prohibit_vector_recycling.MAXLENGTH <- function(...) {
  # http://stackoverflow.com/a/9335687/1664978
  lengths <- vapply(list(...), FUN = length, FUN.VALUE = 0L)
  max.length <- max(lengths)
  if (any(lengths != 1L & lengths != max.length)) {
    i <- which.max(lengths != 1L & lengths != max.length)
    j <- which.max(lengths)
    dots <- eval(substitute(alist(...)))
    first_wrong_arg <- as.character(dots[i])
    max_wrong_arg <- as.character(dots[j])
    stop("`", first_wrong_arg, "` had length ", lengths[i],
         ", but the length of `", max_wrong_arg, "` is ", max.length, ". ",
         "The only permissible vector lengths are 1 or the maximum length of the inputs.")
  } else {
    invisible(max.length)
  }
}





