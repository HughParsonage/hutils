#' Fast selection of \code{data.table} columns
#' @description Present since \code{hutils 1.2.0}.
#' 
#' @param DT A \code{data.table}.
#' @param ... Unquoted columns names.
#' @param cols Character vector of column names.
#' @param preserve.key (logical, default: \code{TRUE}) Reapply the key (if \code{DT} has one)?
#' @param shallow (logical, default: \code{FALSE}) Should the result be a shallow
#' \code{\link[data.table]{copy}} of 
#' \code{DT}'s columns or should the columns be assigned by reference? If \code{TRUE},
#' any modification to the result also modifies the selected columns in \code{DT}.
#' 
#' @return \code{DT} with the selected columns. 
#' 
#' @examples
#' RQ("nycflights13", no = {
#'  library(nycflights13)
#'  library(data.table)
#'  fs <- as.data.table(flights)
#'  fs1 <- selector(fs, year, month, day, arr_delay)
#'  fs1[, arr_delay := NA]
#' })
#' @export


selector <- function(DT, ..., cols = NULL, preserve.key = TRUE, shallow = FALSE) {
  dot_cols <- 
    if (!missing(..1)) {
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
  if (shallow) {
    out
  } else {
    copy(out)
  }
}

