#' Put columns first or last
#' @description Reorder columns of a \code{data.table} (via \code{setcolorder}) so that particular columns 
#' appear first (or last), or in a particular order.
#' @param DT A data.table.
#' @param cols Character vector of columns to put before (after) all others or, in the case of \code{set_colsuborder},
#' a vector of columns in the order requested.
#' @param intersection Use the intersection of the names of \code{DT} and \code{cols}. If \code{FALSE} 
#' any \code{cols} are not the names of \code{DT}, the function may error on behalf of \code{data.table}.
#' Not available for \code{set_colsuborder}.
#' @details In the case of \code{set_colsuborder} the group of columns \code{cols} occupy the same positions
#' in \code{DT} but in a different order. See examples.
#' @examples 
#' 
#' library(data.table)
#'   
#' DT <- data.table(y = 1:5, z = 11:15, x = letters[1:5])
#' set_cols_first(DT, "x")[]
#' set_cols_last(DT, "x")[]
#' set_colsuborder(DT, c("x", "y"))[]
#' 
#' @export set_cols_first set_cols_last set_colsuborder

set_cols_first <- function(DT, cols, intersection = TRUE){
  if (intersection){
    # intersect(x, y) != intersect(y, x)
    return(setcolorder(DT, c(intersect(cols, names(DT)), setdiff(names(DT), cols))))
  } else {
    return(setcolorder(DT, c(cols, setdiff(names(DT), cols))))
  }
}

#' @rdname set_cols_first
set_cols_last <- function(DT, cols, intersection = TRUE){
  if (intersection){
    return(setcolorder(DT, c(setdiff(names(DT), cols), intersect(cols, names(DT)))))
  } else {
    return(setcolorder(DT, c(setdiff(names(DT), cols), cols)))
  }
}

#' @rdname set_cols_first
set_colsuborder <- function(DT, cols, intersection = TRUE) {
  orig_order <- names(DT)
  
  stopifnot(is.character(cols),
            length(cols) > 0,
            isTRUE(intersection),
            all(cols %chin% orig_order))
  
  matches <- chmatch(orig_order, cols)
  new_order <- orig_order
  j <- 0
  for (i in seq_along(new_order)) {
    if (!is.na(matches[i])) {
      j <- j + 1
      new_order[i] <- cols[j]
    }
  }
  
  setcolorder(DT, new_order)
}




