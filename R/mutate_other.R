#' @title Group infrequent entries into 'Other category'
#' @description Useful when you want to constrain the number of unique values in a column by keeping only the most common values.
#' @param .data Data containing variable.
#' @param var Variable containing infrequent entries, to be collapsed into "Other". 
#' @param n Threshold for total number of categories above "Other".
#' @param count Threshold for total count of observations before "Other".
#' @param by Extra variables to group by when calculating \code{n} or \code{count}.
#' @param copy Should \code{.data} be copied? Currently only \code{TRUE} is supported.
#' @param other.category Value that infrequent entries are to be collapsed into. Defaults to \code{"Other"}.
#' @return \code{.data} but with \code{var} changed so that infrequent values have the same value (\code{other.category}).
#' @export 
mutate_other <- function(.data, var, n = 5, count, by = NULL, copy = TRUE, other.category = "Other"){
  stopifnot(is.data.table(.data), 
            is.character(other.category), 
            identical(length(other.category), 1L))
  
  had.key <- haskey(.data)
  
  if (!isTRUE(copy)){
    stop("copy must be TRUE")
  }
  
  out <- copy(.data)
  
  if (had.key){
    orig_key <- key(out)
  } else {
    orig_key <- "_order"
    out[, "_order" := 1:.N]
    setkeyv(out, "_order")
  }
  
  if (is.character(.data[[var]])){
    stopifnot("nvar" %notin% names(.data),
              var %in% names(.data))
    
    N <- .rank <- NULL
    n_by_var <-
      out %>%
      .[, .N, keyby = c(var, by)] %>%
      .[, .rank := rank(-N), by = by]
    
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
    
    setkeyv(out, orig_key)
    
    if (!had.key){
      out[, (orig_key) := NULL]
      setkey(out, NULL)
    }
    out
    
  } else {
    warning("Attempted to use by = on a non-character vector. Aborting.")
    return(.data)
  }
}
