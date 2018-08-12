#' Proportion of values that are NA.
#' @param v A vector.
#' @return A double, \code{mean(is.na(v))}.
#' @export

mean_na <- function(v) {
  if (anyNA(v)) {
    sum(is.na(v)) / length(v)
  } else {
    0
  }
}


