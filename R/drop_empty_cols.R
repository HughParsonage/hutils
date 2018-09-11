#' Drop empty columns
#' @description Removes columns from a \code{data.table} where all the values are missing.
#' @param DT A \code{data.table}.
#' @param copy Copies the \code{data.table} so the original can be retained. 
#' Not applicable if \code{DT} is not a \code{data.table}.
#' If \code{FALSE}, the default, \code{DT} itself will be modified.
#' @export
#' 

drop_empty_cols <- function(DT, copy = FALSE) {
  if (is.data.table(DT)) {
    if (copy) {
      out <- copy(DT)
    } else {
      out <- DT
    }
    
    isEmpty <- vapply(out, function(x) anyNA(x) && all(is.na(x)), logical(1L), USE.NAMES = FALSE)
    
    # Generally safe to delete by names, but not if
    # the names are not distinct
    if (any(isEmpty)) {
      empty_cols <- which(isEmpty, useNames = FALSE)
      out[, (empty_cols) := NULL]
    }
  } else {
    if (!is.data.frame(DT)) {
      stop("`DT` was ", class(DT), " but must be a data.frame.")
    }
    
    if (NEITHER(missing(copy), copy)) {
      warning("`copy` is FALSE, but `DT` is not a data.table, ", 
              "so `DT` will not be modified by reference.\n\n", 
              "Either ensure `DT` is a data.table or assert `copy = TRUE`.")
    }
    
    isEmpty <- vapply(DT, function(x) anyNA(x) && all(is.na(x)), logical(1), USE.NAMES = FALSE)
    if (any(isEmpty)) {
      non_empty_cols <- which(!isEmpty)
      out <- DT[, non_empty_cols, drop = FALSE]
    } else {
      out <- DT
    }
    
  }
  
  out[]
}
