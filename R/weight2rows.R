#' Expand a weighted data frame to an equivalent unweighted
#' @param DT A \code{data.table}. Will be converted to one if possible.
#' @param weight.var Variable in \code{DT} to be used as weights.
#' @return \code{DT} but with the number of rows expanded to \code{sum(DT[[weight.var]])} to reflect the weighting.
#' @examples 
#' 
#' library(data.table)
#' DT <- data.table(x = 1:5, y = c(1, 1, 1, 1, 2))
#' weight2rows(DT, "y")
#' 
#' @export 

weight2rows <- function(DT, weight.var) {
  weight.var.value <- DT[[weight.var]]
  if (any(weight.var.value < 0)) {
    stop("Negative values in weight.var. ",  
         "These are unlikely weights and not readily convertible to extra rows.")
  }
  
  if (!is.data.table(DT)) {
    setDT(DT)
  }
  
  switch(typeof(weight.var.value), 
         "logical" = {
           warning("weight.var is logical. Treating as filter/subset.")
           DT[which(weight.var.value)]
         },
         "integer" = {
           DT %>%
             .[weight.var.value > 0] %>%
             .[, lapply(.SD, rep_len, .BY[[1]] * .N), .SDcols = names(.)[names(.) != weight.var], by = weight.var]
         },
         "double" = {
           DT %>%
             .[weight.var.value > 0] %>%
             .[, lapply(.SD, rep_len, round(.BY[[1]] * .N)), .SDcols = names(.)[names(.) != weight.var], by = weight.var]
         }, 
         stop("Non-numeric weight.var. Aborting."))

}
