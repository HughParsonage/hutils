#' Prohibit unequal length vectors
#' @description Tests whether all vectors have the same length.
#' @param ... Vectors to test.
#' @return An error message unless all of \code{...} have the same length in which case \code{NULL}, invisibly.
#' @export

prohibit_unequal_length_vectors <- function(...) {
  lengths <- lengths(list(...))
  if (max(lengths) != min(lengths)) {
    stop("Input vectors must all have the same length.")
  }
  invisible(NULL)
}

