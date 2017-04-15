#' Return duplicated rows of data.table
#' @param DT A data.table.
#' @param by Columns to evaluate duplicates over. 
#' @param na.rm Remove NA is in \code{by} before returning duplicates. 
#' @param order Order the result so that it duplicates are adjacent.
#' @return Duplicate rows of DT by \code{by}. For interactive use.
#' @export 
#' 
duplicated_rows <- function(DT, by = names(DT), na.rm = FALSE, order = TRUE){
  if (order){
    setorderv(DT, by)
  }
  
  if (na.rm){
    DT %>%
      .[!is.na(DT[[by]])] %>%
      .[duplicated(., by = by) | duplicated(., by = by, fromLast = TRUE)]
  } else {
    DT %>%
      .[duplicated(., by = by) | duplicated(., by = by, fromLast = TRUE)]
  }
}
