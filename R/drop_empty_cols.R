#' Drop empty columns
#' @description Removes columns from a \code{data.table} where all the values are missing.
#' @param DT A \code{data.table}.
#' @param copy Copies the \code{data.table} so the original can be retained. 
#' Not applicable if \code{DT} is not a \code{data.table}.
#' If \code{FALSE}, the default, \code{DT} itself will be modified, not just the value.
#' @export
#' 

drop_empty_cols <- function(DT, copy = FALSE) {
  if (copy) {
    out <- copy(DT)
  } else {
    out <- DT
  }
  
  is_empty <- vapply(out, function(x) all(is.na(x)), logical(1))
  if (is.data.table(out)) {
    for (j in names(is_empty)[is_empty]) {
      out[, (j) := NULL]
    }
  } else {
    out <- dplyr::select_(out, .dots = names(is_empty)[!is_empty])
  }
  out[]
}
