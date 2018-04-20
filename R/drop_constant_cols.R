#' Drop constant columns
#' @description Drops columns that have only one value in a \code{data.table}.
#' @param DT A \code{data.table}.
#' @param copy (logical, default: \code{FALSE}) Whether the \code{data.table} should be copied before any columns are dropped. If \code{FALSE}, the default, columns are dropped from \code{DT} by reference.
#' @details If \code{DT} is a \code{data.frame} that is not a \code{data.table},
#' constant columns are still dropped, but since \code{DT} will be copied, \code{copy} should be set 
#' to \code{TRUE} to avoid a warning. If \code{DT} is a \code{data.frame} and all but one
#' of the columns are constant, a \code{data.frame}
#' will still be returned, as opposed to the values of the sole remaining column, which is the 
#' default behaviour of base \code{data.frame}.
#' 
#' If all columns are constant, \code{drop_constant_cols} returns a Null data table if \code{DT} is a \code{data.table},
#' but a data frame with 0 columns and \code{nrow(DT)} otherwise.
#' 
#' @examples 
#' library(data.table)
#' X <- data.table(x = c(1, 1), y = c(1, 2))
#' drop_constant_cols(X)
#' 
#' 
#' 
#' @export

drop_constant_cols <- function(DT, copy = FALSE) {
  
  
  if (is.data.table(DT)) {
    if (nrow(DT) < 2L) {
      # Rest of function won't work with [2]
      return(data.table())
    }
    if (copy) {
      out <- copy(DT)
    } else {
      out <- DT
    }
    
    constant_cols <-
      vapply(out,
             function(v) {
               identical(v[1L], v[2L]) &&
                 uniqueN(v) == 1L
             },
             FUN.VALUE = logical(1L),
             USE.NAMES = FALSE)
    
    # Generally safe to delete by names, but not if
    # the names are not distinct
    if (any(constant_cols)) {
      const_cols <- which(constant_cols, useNames = FALSE)
      out[, (const_cols) := NULL]
    }
  } else {
    if (!is.data.frame(DT)) {
      stop("`DT` was a ", class(DT), ", but `DT` must be a data.frame.")
    }
    if (nrow(DT) < 2L) {
      # Rest of function won't work with [2]
      return(data.frame())
    }
    
    if (NEITHER(missing(copy), copy)) {
      warning("`copy` is FALSE, yet `DT` is not a data.table, ", 
              "so `DT` will not be modified by reference.\n\n", 
              "Either ensure `DT` is a data.table or assert `copy = TRUE`.")
    }
    
    non_constant_cols <-
      vapply(DT,
             function(v) {
               !identical(v[1L], v[2L]) ||
                 uniqueN(v) > 1L
             },
             FUN.VALUE = logical(1L),
             USE.NAMES = TRUE)
    non_const_cols <- names(non_constant_cols)[non_constant_cols]
    out <- DT[, non_const_cols, drop = FALSE]
    
  }
  
  out[]
  
}

