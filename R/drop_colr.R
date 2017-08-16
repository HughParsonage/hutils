#' Drop columns whose names match a pattern
#' @param DT A \code{data.table}.
#' @param pattern A regular expression as in \code{grepl}.
#' @param ... Arguments passed to \code{grepl}.
#' @param checkDT If \code{TRUE} (the default), will error if \code{DT} is not a \code{data.table}.
#' @export

drop_colr <- function(DT, pattern, ..., checkDT = TRUE) {
  if (checkDT) {
    stopifnot(is.data.table(DT))
  }
  
  DT[, .SD, .SDcols = names(DT)[!grepl(pattern = pattern, x = names(DT), ...)]]
}

