#' Put columns first or last
#' @param DT A data.table.
#' @param cols Columns to put before (after) all others
#' @param intersection Use the intersection of the names of \code{DT} and \code{cols}.
#' @export set_cols_first set_cols_last

set_cols_first <- function(DT, cols, intersection = TRUE){
  if (intersection){
    return(setcolorder(DT, c(intersect(names(DT), cols), setdiff(names(DT), cols))))
  } else {
    return(setcolorder(DT, c(cols, setdiff(names(DT), cols))))
  }
}

#' @rdname set_cols_first
set_cols_last <- function(DT, cols, intersection = TRUE){
  if (intersection){
    return(setcolorder(DT, c(setdiff(names(DT), cols), intersect(names(DT), cols))))
  } else {
    return(setcolorder(DT, c(setdiff(names(DT), cols), cols)))
  }
}


