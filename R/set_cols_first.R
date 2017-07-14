#' Put columns first or last
#' @description Reorder columns of a \code{data.table} (via \code{setcolorder}) so that particular columns 
#' appear first (or last).
#' @param DT A data.table.
#' @param cols Character vector of columns to put before (after) all others.
#' @param intersection Use the intersection of the names of \code{DT} and \code{cols}. If \code{FALSE} 
#' any \code{cols} are not the names of \code{DT}, the function may error on behalf of \code{data.table}.
#' @examples 
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   library(data.table)
#'   
#'   DT <- data.table(y = 1:5, z = 11:15, x = letters[1:5])
#'   set_cols_first(DT, "x")
#' }
#' @export set_cols_first set_cols_last

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


