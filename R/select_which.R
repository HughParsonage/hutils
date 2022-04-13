#' Select columns satisfying a condition
#' @param DT A \code{data.table}.
#' @param Which A function that takes a vector and returns \code{TRUE} or \code{FALSE}. \code{TRUE} columns are selected.
#' @param .and.dots Optional extra columns to include. May be a character vector of \code{names(DT)} or numeric (positions) or logical. If provided, the columns so added (if they do not satisfy \code{Which}) will be after all the columns \code{Which} do so satisfy.
#' @param checkDT If \code{TRUE} (the default), an informative error message is provided if \code{DT} is not a \code{data.table}. 
#' @param .and.grep A character vector of regular expressions to match to the names
#' of \code{DT}. The corresponding columns will be included in the result.
#' @return \code{DT} with the selected variables.
#' @examples 
#' library(data.table)
#' DT <- data.table(x = 1:5,
#'                  y = letters[1:5],
#'                  AB = c(NA, TRUE, FALSE))
#' select_which(DT, anyNA, .and.dots = "y")
#' @export

select_which <- function(DT, Which, .and.dots = NULL, checkDT = TRUE, .and.grep = NULL) {
  if (checkDT && !is.data.table(DT)) {
    stop("`DT` was ", class(DT), "but must be a data.table.")
  }
  
  Which <- match.fun(Which)
  if (is.null(.and.dots) && is.null(.and.grep)) {
    DT[, .SD, .SDcols = names(DT)[vapply(DT, Which, logical(1))]]
  } else {
    sdcols_extra <- 
      if (is.character(.and.dots)) {
        .and.dots
      } else {
        names(DT)[.and.dots]
      }
    
    for (gx in .and.grep) {
      sdcols_extra <- 
        c(sdcols_extra, 
          grep(gx, names(DT), value = TRUE, perl = TRUE))    
    }
    
    
    
    DT[, .SD, .SDcols = union(names(DT)[vapply(DT, Which, logical(1))], sdcols_extra)]
  }
}




