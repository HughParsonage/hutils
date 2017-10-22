#' @title Group infrequent entries into 'Other category'
#' @description Useful when you want to constrain the number of unique values in a column by keeping only the most common values.
#' @param .data Data containing variable.
#' @param var Variable containing infrequent entries, to be collapsed into "Other". 
#' @param n Threshold for total number of categories above "Other".
#' @param count Threshold for total count of observations before "Other".
#' @param by Extra variables to group by when calculating \code{n} or \code{count}.
#' @param var.weight Variable to act as a weight: \code{var}'s where the sum of this variable exceeds 
#' \code{mass} will be kept, others set to \code{other.category}.
#' @param mass Threshold for sum of \code{var.weight}.
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
                         mass = -Inf,
                         copy = TRUE,
                         other.category = "Other"){
  stopifnot(is.data.table(.data), 
            is.character(other.category), 
            identical(length(other.category), 1L))
  
  if (is.character(.data[[var]])){
    had.key <- haskey(.data)
    
    if (!isTRUE(copy)){
      stop("copy must be TRUE")
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
    n_by_var <-
      out %>%
      .[, .N, keyby = c(var, by)] %>%
      .[, .rank := rank(-N), by = by]
    
    if (!is.null(var.weight)) {
      stopifnot(var.weight %chin% names(out),
                !("wEiGhT" %in% names(out))) 
      setnames(out, var.weight, "wEiGhT")
      wEiGhT <- NULL
      wt_by_var <- out[, .(wEiGhT = sum(wEiGhT)), by = c(var, by)]
      setorderv(wt_by_var, "wEiGhT", order = -1L)
      
      if (is.infinite(mass)) {
        warning("mass set to non-finite value, perhaps by default. Choose a better value.")
      }
      
      vars_to_coalesce <- wt_by_var[wEiGhT < mass][[var]]
      
      for (i in which(out[[var]] %fin% vars_to_coalesce)) {
        set(out, i = i, j = var, value = other.category)
      }
      setnames(out, "wEiGhT", var.weight)
    } else {
      
      out <- merge(out, n_by_var, by = c(var, by))
      
      if (missing(count)){
        out[.rank > n, (var) := other.category]
      } else {
        out[N < count, (var) := other.category]
      }
      out <- 
        out %>%
        .[, N := NULL] %>%
        .[, .rank := NULL] 
    }
    
    setkeyv(out, orig_key)
    
    if (!had.key){
      out[, (orig_key) := NULL]
      setkey(out, NULL)
    }
    
    if (has_null_nom_attr && !is.null(attr(names(out), ".match.hash"))) {
      setattr(names(out), ".match.hash", NULL)
    }
    
    out
    
  } else {
    warning("Attempted to use by = on a non-character vector. Aborting.")
    return(.data)
  }
}
