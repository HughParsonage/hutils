#' Drop column or columns
#' @name drop_col
#' @param DT A \code{data.table}.
#' @param var Quoted column to drop.
#' @param checkDT Should the function check \code{DT} is a \code{data.table}?
#' @return \code{DT} with specified columns removed.
#' 
#' @examples 
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   library(data.table)
#'   DT <- data.table(x = 1, y = 2, z = 3)
#'   
#'   drop_col(DT, "x")
#' }
#' 
#' @export drop_col

drop_col <- function(DT, var, checkDT = TRUE) {
  if (checkDT) {
    stopifnot(is.data.table(DT))
  }
  
  if (length(var) != 1) {
    stop("var must be a single column. Use drop_cols().")
  }
  
  if (var %in% names(DT)) {
    DT[, (var) := NULL]
  }
  DT
}


#' @param vars Character vector of columns to drop. Only the intersection is dropped; 
#' if any \code{vars} are not in \code{names(DT)}, no warning is emitted.
#' @rdname drop_col
#' @export drop_cols
drop_cols <- function(DT, vars, checkDT = TRUE) {
  if (checkDT) {
    stopifnot(is.data.table(DT))
  }
  
  common_vars <- intersect(vars, names(DT))

  if (length(common_vars) > 0) {
    DT[, (common_vars) := NULL]
  }
  
  DT
}

