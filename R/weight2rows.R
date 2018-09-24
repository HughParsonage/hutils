#' Expand a weighted data frame to an equivalent unweighted
#' @description Present since \code{v1.0.0}.
#'  Argument \code{rows.out} available since \code{v1.3.0}.
#'  Argument \code{discard_weight.var} available since \code{v 1.3.0}.
#' @param DT A \code{data.table}. Will be converted to one if possible.
#' @param weight.var Variable in \code{DT} to be used as weights.
#' @param rows.out If not \code{NULL} (the default) specifies the number of rows in the result;
#' otherwise the number of rows will be \code{sum(DT[[weight.var]])}. (Due to rounding, this figures are inexact.)
#' @param discard_weight.var If \code{FALSE}, the default, \code{weight.var}
#' in \code{DT} will be \code{1} for each row in the result or a new weight 
#' if \code{rows.out} is given. Otherwise, \code{TRUE} drops the column entirely.
#' 
#' 
#' @return \code{DT} but with the number of rows expanded to \code{sum(DT[[weight.var]])} to reflect the weighting.
#' @examples 
#' 
#' library(data.table)
#' DT <- data.table(x = 1:5, y = c(1, 1, 1, 1, 2))
#' weight2rows(DT, "y")
#' weight2rows(DT, "y", rows.out = 5)
#' 
#' @export 

weight2rows <- function(DT,
                        weight.var,
                        rows.out = NULL,
                        discard_weight.var = FALSE) {
  if (!is.data.table(DT)) {
    if (!is.data.frame(DT)) {
      stop("`DT` was a ", class(DT)[1L], ". ",
           "DT must be a data.frame.")
    }
    DT <- as.data.table(DT)
  }
  check_TF(discard_weight.var)
  
  
  
  the_colorder <- copy(names(DT))
  
  
  if (length(weight.var) != 1L) {
    stop("`weight.var` had length ", length(weight.var), ". ",
         "`weight.var` must be a single column name or position.")
  }
  if (is.numeric(weight.var)) {
    if (weight.var < 1 || weight.var > ncol(DT)) {
      stop("`weight.var = ", weight.var, "` outside the ", 
           "range 1 <= x <= ", ncol(DT), " = ncol(DT).\n",
           "If used to specify a position, `weight.var` must specify a column number.")
    }
    weight.var <- names(DT)[weight.var]
  } else if (is.character(weight.var)) {
    if (weight.var %notchin% names(DT)) {
      stop("`weight.var = ", weight.var, "` but is not a column name of DT.", "\n", 
           "`weight.var` needs to specify a valid column of DT.")
    }
  } else {
   stop("`typeof(weight.var) = '", typeof(weight.var), "'`, ",
        "but must be numeric or character.") 
  }
  
  weight.var.value <- DT[[weight.var]]
  if (anyNA(weight.var.value)) {
    warning("`weight.var` contained NAs. These have been converted to zeroes.")
    weight.var.value <-
      coalesce(weight.var.value,
               if (is.integer(weight.var.value)) 0L else 0.0)
  }
  if (min(weight.var.value) < 0) {
    stop("`weight.var` contains negative values. ",
         "These are unlikely weights and not readily convertible to extra rows. ",
         "Modify `weight.var` so that all the values are nonnegative.")
  }
  
  
  
  if (is.null(rows.out)) {
    M <- 1L
  } else {
    if (!is.numeric(rows.out)) {
      stop("`typeof(rows.out) = '", typeof(rows.out), "'`.\n",
           "`rows.out`, if used, must be a single number.")
    }
    if (length(rows.out) != 1L) {
      stop("`length(rows.out) = ", length(rows.out), "`.\n",
           "`rows.out`, if used, must be a single number.")
    }
    if (anyNA(rows.out)) {
      stop("`rows.out = NA` but NA is not permitted.\n", 
           "`rows.out`, if used, must be a single number.")
    }
    M <- rows.out / sum(weight.var.value)
  }
  
  rep_out <- function(x, BY, N, M) {
    len <- BY * N * M
    if (len < 1) {
      len <- if (runif(1) < len) 1L else 0L
    } else if (is.double(len)) {
      len <- as.integer(round(len))
    }
    rep_len(x, len)
  }
  
  
  
  
    switch(typeof(weight.var.value), 
           "logical" = {
             warning("weight.var is logical. Treating as filter/subset.")
             out <- DT[which(weight.var.value)]
             
             M <- TRUE
           },
           "integer" = {
             out <-  
               DT %>%
               .[weight.var.value > 0] %>%
               .[, lapply(.SD, rep_out, .BY[[1]], .N, M),
                 .SDcols = names(.)[names(.) != weight.var],
                 by = weight.var]
             
             M <- as.integer(M)
           },
           "double" = {
             
             out <- 
               DT %>%
               .[weight.var.value > 0] %>%
               .[, lapply(.SD, rep_out, .BY[[1]], .N, M),
                 .SDcols = names(.)[names(.) != weight.var],
                 by = weight.var]
             
             if (!is.null(rows.out)) {
               M <- as.double(M)
             }
             
           }, 
           stop("Non-numeric weight.var. Aborting."))
  
  # by will fix things first
  setcolorder(out, the_colorder)
  
  if (discard_weight.var) {
    out[, (weight.var) := NULL]
  } else {
    out[, (weight.var) := M]
  }
  
  out[]
  
}
