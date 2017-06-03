#' Drop empty columns
#' @description Removes columns from a \code{data.table} where all the values are missing.
#' @param DT A \code{data.table}.
#'
#' 

drop_empty_cols <- function(DT) {
  is_empty <- vapply(DT, function(x) all(is.na(x)), logical(1))
  if (is.data.table(DT)) {
    for (j in names(is_empty)[is_empty]) {
      DT[, (j) := NULL]
    }
  } else {
    DT <- select_(DT, .dots = names(is_empty)[!is_empty])
  }
  DT
}
