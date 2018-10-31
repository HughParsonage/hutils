#' Longest common prefix/suffix
#' @name longest_affix
#' @param x A character vector.
#' @param .x If \code{NULL}, the default, ignored. May be used if \code{x} is
#' known to be free of \code{NA}s.
#' @param na.rm (logical, default: \code{TRUE}) If \code{FALSE}, an \code{NA} in 
#' \code{x} means \code{""} is the only common affix. If \code{NA}, the longest 
#' prefix/suffix is \code{NA_character_} (provided \code{anyNA(x)}).
#' 
#' If \code{anyNA(x) == FALSE} \code{na.rm} has no effect.
#' 
#' @param prefixes (logical, default: \code{TRUE}) If \code{TRUE}, trim prefixes.
#' @param suffixes (logical, default: \code{TRUE}) If \code{TRUE}, trim suffixes.
#' 
#' @param warn_if_no_prefix,warn_if_no_suffix (logical, default: \code{TRUE})
#' If \code{FALSE}, if \code{x} has no common affixes the warning is suppressed.
#' (If no common prefix/suffix then the common affix returned will be \code{""}
#' (the empty string).)
#' 
#' @return 
#' The longest common substring in \code{x} either at the start or end of each string.
#' For \code{trim_common_affixes} \code{x} with common prefix and common suffix
#' removed.
#' 
#' 
#' @examples
#' longest_prefix(c("totalx", "totaly", "totalz"))
#' longest_suffix(c("ztotal", "ytotal", "xtotal"))
#' @export longest_suffix longest_prefix trim_common_affixes

trim_common_affixes <- function(x, .x = NULL, na.rm = TRUE,
                                prefixes = TRUE,
                                suffixes = TRUE,
                                warn_if_no_prefix = TRUE,
                                warn_if_no_suffix = TRUE) {
  if (is.null(.x)) {
    if (is.null(x)) {
      return(character(0L))
    }
    if (anyNA(x)) {
      .x <- unique(x[complete.cases(x)])
    } else {
      .x <- unique(x)
    }
  }
  Prefix <- 
    if (prefixes) {
      longest_prefix(.x = .x, na.rm = na.rm, warn_if_no_prefix = warn_if_no_prefix)
    } else {
      ""
    }
  Suffix <-
    if (suffixes) {
      longest_suffix(.x = .x, na.rm = na.rm, warn_if_no_suffix = warn_if_no_suffix)
    } else {
      ""
    }
  if (length(Prefix) == 0L &&
      length(Suffix) == 0L) {
    return(x)
  }
  
  # Need to iterate over BY[[1L]] since substr is not vectorized.
  o1 <- setDT(list(v = x, ncharv = nchar(x)))
  res <- v <- NULL
  o1[, "res" := substr(v, nchar(Prefix) + 1L, .BY[[1L]] - nchar(Suffix)), 
     by = "ncharv"]
  .subset2(o1, "res")
}


#' @rdname longest_affix
longest_suffix <- function(x, .x = NULL, na.rm = TRUE,
                           warn_if_no_suffix = TRUE) {
  if (is.null(.x)) {
    if (anyNA(x)) {
      if (!is.logical(na.rm)) {
        stop("`na.rm` was type ", class(na.rm), ", but must be logical. ",
             "`na.rm` must be NA, FALSE, or TRUE.")
      }
      if (length(na.rm) != 1L) {
        stop("`na.rm` was length-", length(na.rm), ", but must be length-one. ",
             "`na.rm` must be NA, FALSE, or TRUE.")
      }
      
      if (anyNA(na.rm)) {
        return(NA_character_)
      } else if (na.rm) {
        x <- unique(x[complete.cases(x)])
      } else {
        return("")
      }
    } else {
      x <- unique(x)
    }
  } else {
    x <- .x
  }
  if (!length(x)) {
    return(character(0L))
  }
  x1 <- x[1]
  nchar1 <- nchar(x1)
  if (nchar1 <= 1L) {
    if (warn_if_no_suffix) {
      warning("No common suffix.")
    }
    return("")
  }
  for (k in 1:nchar1) {
    suffix <- substr(x1, k, nchar1)
    for (i in seq_along(x)) {
      if (!endsWith(x[i], suffix)) {
        break
      }
      if (i == length(x)) {
        return(suffix)
      }
    }
  }
  ""
}

#' @rdname longest_affix
longest_prefix <- function(x, .x = NULL, na.rm = TRUE,
                           warn_if_no_prefix = TRUE) {
  if (is.null(.x)) {
    if (anyNA(x)) {
      if (!is.logical(na.rm)) {
        stop("`na.rm` was type ", class(na.rm), ", but must be logical. ",
             "`na.rm` must be NA, FALSE, or TRUE.")
      }
      if (length(na.rm) != 1L) {
        stop("`na.rm` was length-", length(na.rm), ", but must be length-one. ",
             "`na.rm` must be NA, FALSE, or TRUE.")
      }
      
      if (anyNA(na.rm)) {
        return(NA_character_)
      } else if (na.rm) {
        x <- unique(x[complete.cases(x)])
      } else {
        return("")
      }
    } else {
      x <- unique(x)
    }
  } else {
    x <- .x
  }
  if (!length(x)) {
    return(character(0L))
  }
  x1 <- x[1L]
  nchar1 <- nchar(x1)
  if (nchar1 <= 1L) {
    if (warn_if_no_prefix) {
      warning("No common prefix.")
    }
    return("")
  }
  for (k in nchar1:1) {
    prefix <- substr(x1, 1L, k)
    if (all(startsWith(x, prefix))) {
      return(prefix)
    }
  }
  ""
}
