#' Return duplicated rows of data.table
#' @description This function differs from \code{duplicated} in that it returns both the duplicate row and the row which has been duplicated.
#' This may prove useful in combination with the \code{by} argument for determining whether two observations are identical across
#' more than just the specified rows. 
#' @param DT A \code{data.table}.
#' @param by Character vector of columns to evaluate duplicates over. 
#' @param na.rm (logical) Should \code{NA}s in \code{by} be removed before returning duplicates? (Default \code{FALSE}.)
#' @param order (logical) Should the result be ordered so that duplicate rows are adjacent? (Default \code{TRUE}.)
#' @param copyDT (logical) Should \code{DT} be copied prior to detecting duplicates. If \code{FALSE}, the ordering of \code{DT} will be changed by reference.
#' @param na.last (logical) If \code{order} is TRUE, should \code{NA}s be ordered first or last?. Passed to \code{data.table::setorderv}.
#' @importFrom stats complete.cases
#' @return Duplicate rows of \code{DT} by \code{by}. For interactive use.
#' 
#' @examples 
#' 
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   library(data.table)
#'
#'   DT <- data.table(x = rep(1:4, 3),
#'                    y = rep(1:2, 6),
#'                    z = rep(1:3, 4))
#' 
#'   # No duplicates
#'   duplicated_rows(DT)
#' 
#'   # x and y have duplicates
#'   duplicated_rows(DT, by = c("x", "y"), order = FALSE)
#' 
#'   # By default, the duplicate rows are presented adjacent to each other.
#'   duplicated_rows(DT, by = c("x", "y"))
#' }
#' 
#' @export 
#' 
duplicated_rows <- function(DT, by = names(DT), na.rm = FALSE, order = TRUE, copyDT = TRUE, na.last = FALSE){
  if (copyDT) {
    DT <- copy(DT)
  }
  
  if (order){
    setorderv(DT, by, na.last = na.last)
  }
  
  if (na.rm){
    DT %>%
      .[complete.cases(DT[, .SD, .SDcols = by])] %>%
      .[duplicated(., by = by) | duplicated(., by = by, fromLast = TRUE)]
  } else {
    DT %>%
      .[duplicated(., by = by) | duplicated(., by = by, fromLast = TRUE)]
  }
}
