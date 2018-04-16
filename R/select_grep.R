#' Select names matching a pattern
#' 
#' @param DT A \code{data.frame}.
#' @param patterns Regular expressions to be matched against the names of \code{DT}. If \code{length(patterns) > 1} the patterns are concatenated using alternation.
#' @param .and Character or integer positions of names to select, regardless of whether or not they are matched by \code{patterns}.
#' @param .but.not Character or integer positions of names to drop, regardless of whether or not they are matched by \code{patterns} or whether they are explicitly added by \code{.and}.
#' @return \code{DT} with the selected names. 
#' @export

select_grep <- function(DT, patterns, .and = NULL, .but.not = NULL) {
  if (not_DT <- !is.data.table(DT)) {
    if (!is.data.frame(DT)) {
      stop("`DT` had class ", paste(class(DT), collapse = " "), ", but must be a data.frame. ",
           "Change `DT`` to be a data.frame.")
    } else {
      DT <- as.data.table(DT)
    }
  }
  if (missing(patterns) || length(patterns) == 0L) {
    stop("`patterns` is missing or length-zero, with no default.")
  } else if (!is.character(patterns)) {
    stop("`patterns` is not a character vector.")
  } else if (length(patterns) == 1L) {
    pattern <- patterns
  } else if (length(patterns) > 1L) {
    pattern <- paste0(patterns, collapse = "|")
  }
  
  noms <- names(DT)
  
  # Require integer positions
  selected <- seq_along(noms)
  
  if (!is.null(.but.not)) {
    switch (typeof(.but.not),
            "logical" = {
              .but.not <- which(.but.not)
            },
            "integer" = {
              .but.not <- .but.not
            },
            "double" = {
              .but.not <- as.integer(.but.not)
            },
            "character" = {
              .but.not <- which(noms %chin% .but.not)
            },
            stop("`.but.not = ", deparse(substitute(.but.not)), "` was type ", 
                 typeof(.but.not),
                 ", but must be either logical, integer, double, or character ",
                 "to identify column positions. ",
                 "Change `.but.not` to one of these types, ",
                 "or choose `.but.not = NULL` if no columns need to be dropped."))
            
    if (length(.but.not)) {
      # Cover instances where .but.not = integer(0)
      selected <- selected[-.but.not]
    }
  }
  
  selected_grep <- grep(pattern, noms, perl = TRUE, value = FALSE)
  
  if (!is.null(.and)) {
    switch (typeof(.and),
            "logical" = {
              .and <- which(.and)
            },
            "integer" = {
              .and <- .and
            },
            "double" = {
              .and <- as.integer(.and)
            },
            "character" = {
              .and <- which(noms %chin% .and)
            },
            stop("`.and = ", deparse(substitute(.and)), "` was type ", 
                 typeof(.and),
                 ", but must be either logical, integer, double, or character ", 
                 "to identify column positions. ",
                 "Change `.and` to one of these types, ",
                 "or choose `.and = NULL` if no columns need to be added."))
    
  }
  # Do not alter positions
  selected <- selected[selected %in% c(selected_grep, .and)]
  
  if (!length(selected)) {
    return(data.table())
  }
    
  out <- DT[, .SD, .SDcols = c(selected)]
  
  if (not_DT) {
    out <- setDF(out)
  }
  out
}
