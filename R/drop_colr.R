#' @title Drop columns whose names match a pattern
#' @name drop_colr
#' @aliases drop_grep
#' @param DT A \code{data.table}.
#' @description \code{drop_colr} present since \code{hutils 1.0.0}.
#' 
#' \code{drop_grep} is identical but only present since \code{hutils 1.2.0}.
#' @param pattern A regular expression as in \code{grepl}.
#' @param ... Arguments passed to \code{grepl}.
#' @param checkDT If \code{TRUE} (the default), will error if \code{DT} is not a \code{data.table}.
#' 
#' @examples
#' library(data.table)
#' dt <- data.table(x1 = 1, x2 = 2, y = 3)
#' drop_grep(dt, "x")
#' 
#' 
#' @export drop_colr drop_grep

drop_colr <- function(DT, pattern, ..., checkDT = TRUE) {
  if (checkDT) {
    stopifnot(is.data.table(DT))
  }
  
  DT[, .SD, .SDcols = names(DT)[!grepl(pattern = pattern, x = names(DT), ...)]]
}

drop_grep <- drop_colr

