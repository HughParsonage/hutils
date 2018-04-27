#' Select names matching a pattern
#' 
#' @param DT A \code{data.frame}.
#' @param patterns Regular expressions to be matched against the names of \code{DT}. If \code{length(patterns) > 1} the patterns are concatenated using alternation.
#' @param .and Character or integer positions of names to select, regardless of whether or not they are matched by \code{patterns}.
#' @param .but.not Character or integer positions of names to drop, regardless of whether or not they are matched by \code{patterns} or whether they are explicitly added by \code{.and}.
#' @param ignore.case,perl,fixed,useBytes,invert Arguments passed to \code{\link[base]{grep}}. Note that \code{perl = TRUE} by default (unlike \code{grep}) unless \code{fixed = TRUE} (and \code{perl} is missing).
#' @param .warn.fixed.mismatch (logical, default: \code{TRUE}) If \code{TRUE}, the default, selecting \code{fixed = TRUE} with \code{perl = TRUE} or \code{ignore.case = TRUE} results in \code{perl} and \code{ignore.case} being reset to \code{FALSE} with a warning (as in \code{grep}), even if it makes no difference to the columns eventually selected. If \code{FALSE} unambiguous results are allowed; if \code{ignore.case = TRUE} and \code{fixed = TRUE}, the result is \strong{unambiguous} if \code{select_grep(DT, tolower(patterns), fixed = TRUE)} and \code{select_grep(DT, toupper(patterns), fixed = TRUE)} are identical.
#' @return \code{DT} with the selected names. 
#' 
#' @examples
#' library(data.table)
#' dt <- data.table(x1 = 1, x2 = 2, y = 0)
#' select_grep(dt, "x")
#' select_grep(dt, "x", .and = "y")
#' select_grep(dt, "x", .and = "y", .but.not = "x2")
#' 
#' @export

select_grep <- function(DT, patterns, .and = NULL, .but.not = NULL,
                        ignore.case = FALSE,
                        perl = TRUE, fixed = FALSE, useBytes = FALSE,
                        invert = FALSE, 
                        .warn.fixed.mismatch = TRUE) {
  TFs <- areTrueFalse(ignore.case, perl, fixed, useBytes, invert, .warn.fixed.mismatch)
  if (!all(TFs)) {
    args <- c("ignore.case", "perl", "fixed", "useBytes", "invert", ".warn.fixed.mismatch")
    if (sum(!TFs) == 1L) {
      stop("`",
           args[!TFs],
           "` had a value other than TRUE or FALSE. Set to TRUE or FALSE.")
    } else {
      stop("The following arguments:\n\t",
           paste0(args[!TFs],
                  collapse = "\n\t"),
           "had a value other than TRUE or FALSE. Set to TRUE or FALSE.")
    }
  }
  
  if (fixed && missing(perl)) {
    perl <- FALSE
  }
  
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
  
  #' @return integer vector of positions
  selected_grep <- 
    if (fixed) {
      if (perl || ignore.case) {
        cols_if_fixed_false <-
          grep(pattern,
               noms,
               perl = perl,
               fixed = FALSE,
               useBytes = useBytes,
               invert = invert,
               ignore.case = ignore.case)
        
        cols_if_fixed_true <-
          if (ignore.case) {
            if (length(patterns) > 1L) {
              union({ 
                toupper(patterns) %>%
                  vapply(grepl, x = noms, fixed = TRUE, FUN.VALUE = logical(length(noms))) %>%
                  rowSums %>%
                  is_greater_than(0L) %>%
                  which
              },
              {
                tolower(patterns) %>%
                  vapply(grepl, x = noms, fixed = TRUE, FUN.VALUE = logical(length(noms))) %>%
                  rowSums %>%
                  is_greater_than(0L) %>%
                  which
              })
            } else {
              union(grep(tolower(pattern),
                         noms,
                         perl = FALSE,
                         fixed = TRUE,
                         useBytes = useBytes,
                         invert = invert,
                         ignore.case = FALSE),
                    grep(toupper(pattern),
                         noms,
                         perl = FALSE,
                         fixed = TRUE,
                         useBytes = useBytes,
                         invert = invert,
                         ignore.case = FALSE))
            }
          } else {
            grep(pattern,
                 noms,
                 perl = FALSE,
                 fixed = TRUE,
                 useBytes = useBytes,
                 invert = invert,
                 ignore.case = FALSE)
          }
        
        different_cols <- 
          noms[xor(seq_along(noms) %in% cols_if_fixed_false,
                   seq_along(noms) %in% cols_if_fixed_true)]
        
        if (length(different_cols) > 0L) {
          if (.warn.fixed.mismatch) {
            cols_warning_msg <-
              paste0("This may lead to the following columns being ",
                     "selected or dropped when the opposite was intended:\n\t", 
                     if (length(different_cols) > 10L) {
                       paste0(paste0(head(different_cols), collapse = "\n\t"), 
                              "(first 6 shown).")
                     } else {
                       paste0(different_cols, collapse = "\n\t")
                     })
            
            if (perl) {
              perl <- FALSE
              if (ignore.case) {
                ignore.case <- FALSE
                warning("Changing arguments `perl` and `ignore.case` to FALSE since ",
                        "`fixed = TRUE`. ", 
                        cols_warning_msg, "\n",
                        "Ensure `perl = FALSE` and `ignore.case = FALSE` if `fixed = TRUE`.")
                
              } else {
                warning("Changing argument `perl` to FALSE since ",
                        "`fixed = TRUE`. ",
                        cols_warning_msg, "\n",
                        "Ensure `perl = FALSE` if `fixed = TRUE`.")
              }
            } else {
              if (ignore.case) {
                ignore.case <- FALSE
                warning("Changing argument `ignore.case` to FALSE since ",
                        "`fixed = TRUE`. ",
                        cols_warning_msg, "\n",
                        "Ensure `ignore.case = FALSE` if `fixed = TRUE`.")
              }
            }
          } else {
            grep(pattern,
                 noms,
                 perl = FALSE,
                 value = FALSE,
                 fixed = TRUE,
                 useBytes = useBytes,
                 invert = invert,
                 ignore.case = FALSE)
          }
        } else {
          if (.warn.fixed.mismatch) {
            if (ignore.case && perl) {
              warning("Changing arguments `perl` and `ignore.case` to FALSE since ",
                      "`fixed = TRUE`. ", 
                      "This can lead to wrong columns being selected or dropped. ",
                      "Ensure `perl = FALSE` and `ignore.case = FALSE` if `fixed = TRUE`.")
              
            } else if (perl) {
              warning("Changing `perl` to FALSE since ",
                      "`fixed = TRUE`. ", 
                      "This can lead to wrong columns being selected or dropped. ",
                      "Ensure `perl = FALSE` and `ignore.case = FALSE` if `fixed = TRUE`.")
            } else if (ignore.case) {
              warning("Changing `ignore.case` to FALSE since ",
                      "`fixed = TRUE`. ", 
                      "This can lead to wrong columns being selected or dropped. ",
                      "Ensure `perl = FALSE` and `ignore.case = FALSE` if `fixed = TRUE`.")
            } else stop("Internal error: select_grep:231.")
          }
          cols_if_fixed_true
        }
      } else {
        if (length(patterns) > 1L) {
          vapply(patterns,
                 grepl, x = noms, fixed = TRUE, FUN.VALUE = logical(length(noms))) %>%
            rowSums %>%
            is_greater_than(0L) %>%
            which
        } else {
          grep(pattern, noms,
               perl = perl,
               value = FALSE,
               fixed = fixed,
               useBytes = useBytes,
               invert = invert,
               ignore.case = ignore.case)
        }
      }
    } else {
      grep(pattern, noms,
           perl = perl,
           value = FALSE,
           fixed = fixed,
           useBytes = useBytes,
           invert = invert,
           ignore.case = ignore.case)
    }
  
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






