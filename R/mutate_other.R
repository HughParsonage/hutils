#' @title Group infrequent entries into 'Other category'
#' @description Useful when you want to constrain the number of unique values in a column by keeping only the most common values.
#' @param .data Data containing variable.
#' @param var Variable containing infrequent entries, to be collapsed into "Other". 
#' @param n Threshold for total number of categories above "Other".
#' @param count Threshold for total count of observations before "Other".
#' @param by Extra variables to group by when calculating \code{n} or \code{count}.
#' @param var.weight Variable to act as a weight: \code{var}'s where the sum of this variable exceeds 
#' \code{mass} will be kept, others set to \code{other.category}.
#' @param mass Threshold for sum of \code{var.weight}: any \code{var} where the aggregated sum of \code{var.weight} exceeds \code{mass} will be kept and other \code{var} will be set to \code{other.category}. By default (\code{mass = NULL}), 
#' the value of \code{mass} is \eqn{-\infty}, with a warning. You may set it explicitly to \code{-Inf} if you really want to avoid a warning that this function will have no effect.
#' @param copy Should \code{.data} be copied? Currently only \code{TRUE} is supported.
#' @param other.category Value that infrequent entries are to be collapsed into. Defaults to \code{"Other"}.
#' @return \code{.data} but with \code{var} changed so that infrequent values have the same value (\code{other.category}).
#' @examples 
#' library(data.table)
#' library(magrittr)
#' 
#' DT <- data.table(City = c("A", "A", "B", "B", "C", "D"),
#'                  value = c(1, 9, 4, 4, 5, 11))
#' 
#' DT %>%
#'   mutate_other("City", var.weight = "value", mass = 10) %>%
#'   .[]
#'   
#' @export 
mutate_other <- function(.data,
                         var,
                         n = 5, 
                         count,
                         by = NULL,
                         var.weight = NULL,
                         mass = NULL,
                         copy = TRUE,
                         other.category = "Other"){
  stopifnot(is.data.table(.data), 
            is.character(other.category), 
            identical(length(other.category), 1L))
  
  if (is.character(.data[[var]])){
    had.key <- haskey(.data)
    
    if (!isTRUE(copy)){
      stop("`copy` was ", copy, ", but must be TRUE.")
    }
    
    out <- copy(.data)
    
    has_null_nom_attr <- is.null(attr(names(out), ".match.hash"))
    
    if (had.key){
      orig_key <- key(out)
    } else {
      orig_key <- "_order"
      out[, "_order" := seq_len(.N)]
      setkeyv(out, "_order")
    }
    
    
    # Must be names(out), not names(DT) 
    # otherwise will affect DT
    stopifnot(!("nvar" %in% names(out)),
              var %in% names(out))
    
    
    N <- .rank <- NULL
    if ("N" %chin% names(out)) {
      if ("_temp" %chin% names(out)) {
        `_temp` <- NULL
        new_nom <- paste0(names(out), collapse = "x")
        
        n_by_var <-
          out %>%
          .[, stats::setNames(list(`_temp` = .N), nm = new_nom), keyby = c(var, by)] %>%
          setorderv(c(by, new_nom)) %>%
          .[, .rank := seq_len(.N), by = by]
      } else {
        n_by_var <-
          out %>%
          .[, .(`_temp` = .N), keyby = c(var, by)] %>%
          .[, .rank := rank(-`_temp`), by = by]
      }
    } else {
      n_by_var <-
        out %>%
        .[, .N, keyby = c(var, by)] %>%
        .[, .rank := rank(-N), by = by]
    }
    
    if (!is.null(var.weight)) {
      stopifnot(var.weight %chin% names(out))
      
      # Poor man's SE
      if ("wEiGhT" %in% names(out)) {
        stop("`data` contained column `wEiGhT`, but this is not allowed ",
             "as it conflicts with `mutate_other` internals. ",
             "Rename this column (temporarily at least) to use `mutate_other`.")
      }
      
      if (".rank" %in% names(out)) {
        stop("`data` contained column `.rank`, but this is not allowed ",
             "as it conflicts with `mutate_other` internals. ",
             "Rename this column (temporarily at least) to use `mutate_other`.")
      }
      
      
      setnames(out, var.weight, "wEiGhT")
      wEiGhT <- NULL
      
      wt_by_var <- out[, .(wEiGhT = sum(wEiGhT)), keyby = c(var, by)]
      setorderv(wt_by_var, "wEiGhT", order = -1L)
      wt_by_var[, .rank := seq_len(.N), by = by]
      
      # Two choices, either by 'mass' or by .rank
      if (!missing(mass) && !is.null(n)) {
        warning("`mass` was provided, yet `n` was not set to NULL. ", 
                "As a result, `mass` may be misinterpreted. ",
                "If you intended to use `mass` to create the other category, ", 
                "set `n = NULL`. Otherwise, do not provide `mass`.")
      }
      
      if (is.null(n)) {
        if (is.null(mass)) {
          warning("Setting mass to negative infinity. Choose a better value.")
          mass <- -Inf
        }
        
        # .rank not used, so delete. Set wEiGhT in out
        # back to its original name to avoid i.wEiGhT
        wt_by_var[, .rank := NULL]
        out <- wt_by_var[out, on = c(var, by)]
        out[wEiGhT < mass, (var) := other.category]
        out[, wEiGhT := NULL]
        setnames(out, "i.wEiGhT", var.weight)
        
      } else {
        # wEiGhT has no bearing here, we only care about rank
        # We delete it here to avoid i.wEiGhT in the subsequent 
        # join. 
        wt_by_var[, wEiGhT := NULL]
        out <- wt_by_var[out, on = c(var, by)]
        out[.rank > n, (var) := other.category]
        out[, .rank := NULL]
        setnames(out, "wEiGhT", var.weight)
      }
      
      
    } else {
      out <- n_by_var[out, on = c(var, by)]
      
      if (missing(count) || is.null(count)) {
        out[.rank > n, (var) := other.category]
      } else {
        out[N < count, (var) := other.category]
      }
      out[, N := NULL]
      out[, .rank := NULL] 
    }
    
    setkeyv(out, orig_key)
    
    if (!had.key){
      out[, (orig_key) := NULL]
      setkey(out, NULL)
    }
    
    out
    
  } else {
    warning("`by` was not a character vector, so `mutate_other()` will have no effect.")
    return(.data)
  }
}
