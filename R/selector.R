#' Fasting selection of \code{data.table} columns
#' @description Present since \code{hutils 1.2.0}.
#' 
#' @param DT A \code{data.table}.
#' @param ... Unquoted columns names.
#' @param cols Character vector of column names.
#' @param preserve.key Whether to reapply the key, if \code{DT} has one.
#' 
#' @return \code{DT} with the selected columns. 
#' 
#' @export


selector <- function(DT, ..., cols = NULL, preserve.key = TRUE) {
  dot_cols <- 
    if (...length()) {
      vapply(substitute(...()), deparse, character(1L))
    }
  
  if (is.numeric(cols)) {
    cols <- names(DT)[cols]
  }
  
  if (is.null(dot_cols) && is.null(cols)) {
    return(data.table())
  }
  
  dt_key <- if (preserve.key) key(DT)
  out <- setDT(lapply(c(dot_cols, cols), function(v) .subset2(DT, v)))
  setnames(out, c(dot_cols, cols))
  if (!is.null(dt_key) && all(dt_key %chin% c(dot_cols, cols))) {
    setattr(out, "sorted", dt_key)
  }
  out
}


