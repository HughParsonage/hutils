#' Prohibit unequal length vectors
#' @description Tests whether all vectors have the same length.
#' @param ... Vectors to test.
#' @return An error message unless all of \code{...} have the same length in which case \code{NULL}, invisibly.
#' @export

prohibit_unequal_length_vectors <- function(...) {
  lengths <- lengths(list(...))
  if (max(lengths) != min(lengths)) {
    max.length <- max(lengths)
    i <- which.max(lengths != max.length)
    j <- which.max(lengths)
    dots <- eval(substitute(alist(...)))
    first_wrong_arg <- as.character(dots[i])
    max_wrong_arg <- as.character(dots[j])
    stop("`", first_wrong_arg, "` had length ", lengths[i],
         ", but the length of `", max_wrong_arg, "` is ", max.length, ". ",
         "Each vector must have the same length.")
  }
  invisible(NULL)
}

