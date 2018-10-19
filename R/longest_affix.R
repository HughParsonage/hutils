#' Longest common prefix/suffix
#' @name longest_affix
#' @param x A character vector.
#' @param .x If \code{NULL}, the default, ignored. May be used if \code{x} is
#' known to be free of \code{NA}s.
#' @return The longest common substring in \code{x} either at the start or end of each string.
#' For \code{trim_common_affixes} \code{x} with common prefix and common suffix
#' removed.
#' @examples
#' longest_prefix(c("totalx", "totaly", "totalz"))
#' longest_suffix(c("ztotal", "ytotal", "xtotal"))
#' @export longest_suffix longest_prefix trim_common_affixes

trim_common_affixes <- function(x, .x = NULL) {
  if (is.null(.x)) {
    if (anyNA(x)) {
      x <- unique(x[complete.cases(x)])
    } else {
      x <- unique(x)
    }
  } else {
    x <- .x
  }
  Prefix <- longest_prefix(.x = x)
  Suffix <- longest_suffix(.x = x)
  substr(x, nchar(Prefix) + 1L, nchar(x) - nchar(Suffix))
}


#' @rdname longest_affix
longest_suffix <- function(x, .x = NULL) {
  if (is.null(.x)) {
    if (anyNA(x)) {
      x <- unique(x[complete.cases(x)])
    } else {
      x <- unique(x)
    }
  } else {
    x <- .x
  }
  if (!length(x)) {
    return("")
  }
  x1 <- x[1]
  nchar1 <- nchar(x1)
  if (nchar1 <= 1) {
    warning("No common suffix.")
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
longest_prefix <- function(x, .x = NULL) {
  if (is.null(.x)) {
    if (anyNA(x)) {
      x <- unique(x[complete.cases(x)])
    } else {
      x <- unique(x)
    }
  } else {
    x <- .x
  }
  if (!length(x)) {
    return("")
  }
  x1 <- x[1]
  nchar1 <- nchar(x1)
  if (nchar1 <= 1) {
    warning("No common suffix.")
    return("")
  }
  for (k in 1:nchar1) {
    prefix <- substr(x1, 1, k)
    for (i in seq_along(x)) {
      if (startsWith(x[i], prefix)) {
        next
      } else {
        return(substr(x1, 1, k - 1))
      }
      if (i == length(x)) {
        return(prefix)
      }
    }
  }
  ""
}
