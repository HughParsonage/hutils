#' Pseudo memoize assignment by refernece
#' @description When a value is repeated often in a large \code{data.table}, it may be more efficient to 
#' only perform the operation on the unique values.
#' @param DT A \code{data.table}.
#' @param new_var Name of the new variable.
#' @param old_var Variable to be passed to \code{f_old_var}.
#' @param f_old_var Function to convert \code{old_var} to \code{new_var}.
#' @return \code{DT} with the update variable. 
#' 
#' @examples 
#' dt <- data.table(
#'   DateChar = as.character(as.Date("2000-01-01") + sample(10, replace = TRUE, size = 1e6))
#' )
#' system.time(dt[, Date1 := as.Date(DateChar)])
#' system.time(uniqueLookup(dt, "Date2", "DateChar", as.Date))
#' @export

uniqueLookup <- function(DT, new_var, old_var, f_old_var, by = NULL) {
  f_old_var <- match.fun(f_old_var)
  uniqueOld <- unique(DT[, .SD, .SDcols = c(old_var)])
  setnames(uniqueOld, old_var, "oldvar__oldvar")
  uniqueOld[, "newvar__newvar" := f_old_var(oldvar__oldvar)]
  setnames(uniqueOld, "oldvar__oldvar", old_var)
  DT[uniqueOld, (new_var) := i.newvar__newvar, on = old_var][]
}


