#' Vectorized switch
#' @description Present since \code{hutils 1.2.0}. Vectorized version of \code{switch}. Used to avoid or make clearer the result of 
#' \code{if_else(Expr == , ..1, if_else(Expr == , ..2, ...))}
#' @param Expr A character vector.
#' @param ... As in \code{\link[base]{switch}}, a list of named alternatives.
#' Unlike \code{switch}, unnamed vectors are taken to match \code{""}. Likewise, 
#' \code{NA} values in \code{Expr} must be assigned via \code{IF_NA}.
#' 
#' @param DEFAULT A mandatory default value should any name of \code{...} be left unmatched.
#' @param IF_NA Optional value to replace missing (\code{NA_character_}) values in \code{Expr}.
#' @param MUST_MATCH (logical, default: \code{FALSE}) Must every value in \code{Expr} be matched by a conversion in \code{...}? 
#' If \code{TRUE} any output equal to the value of \code{DEFAULT} is an error.
#' @return For every element of \code{...} whose name matches an element of \code{Expr}, 
#' that element's value.
#' 
#' @examples
#' Switch(c("a", "b", "c", "a"),
#'        "a" = 1, 
#'        "b" = 2, 
#'        "c" = 3, 
#'        "4" = 4, 
#'        DEFAULT = 0)
#' 
#' @export


Switch <- function(Expr, ..., DEFAULT, IF_NA = NULL,
                   MUST_MATCH = FALSE) {
  if (length(Expr) == 1L) {
    return(switch(Expr, ...))
  }
  
  if (!is.character(Expr)) {
    stop("`Expr` was type ", typeof(Expr), ", but must be a character vector.")
  }
  
  if (missing(DEFAULT)) {
    stop("`DEFAULT` is missing with no default.")
  }
  
  if (length(DEFAULT) != 1L &&
      length(DEFAULT) != length(Expr)) {
    stop("`length(DEFAULT) = ", length(DEFAULT), "` ", 
         " but `length(Expr) = ", length(Expr), ". ", 
         "Ensure `DEFAULT` has the same length as `Expr`.")
  }
  
  check_TF(MUST_MATCH)
  
  out <- rep_len(DEFAULT, length(Expr))
  
  typeof_out <- 
    if (is.factor(out)) {
      stop("`DEFAULT` is a factor, not currently supported.")
    } else {
      typeof(out)
    }
  
  if (MUST_MATCH) {
    matches <- integer(length(out))
  }
  
  if (!is.null(IF_NA) && anyNA(Expr)) {
    wis.na <- which(is.na(Expr))
    if (length(IF_NA) == 1L) {
      out[wis.na] <- IF_NA
    } else if (length(IF_NA) == length(out)) {
      out[wis.na] <- IF_NA[wis.na]
    }
    if (MUST_MATCH) {
      matches[wis.na] <- 1L
    }
  }
  
  dots <- list(...)
  dot_noms <- names(dots)
  
  
  for (n in seq_along(dots)) {
    w <- which(Expr == dot_noms[n])
    if (length(w)) {
      if (MUST_MATCH) {
        matches[w] <- matches[w] + 1L
      }
      n_res <- switch(n, ...)
      if (is.factor(n_res)) {
        stop("Argument number ", n, " named '", dot_noms[n], "' was a factor, ",
             "not currently supported.")
      }
      
      if (typeof(n_res) != typeof_out) {
        stop("Argument number ", n, " named '", dot_noms[n], "' was type ", 
             typeof(n_res), " but `typeof(DEFAULT) = ", typeof(DEFAULT), "`. ", 
             "All entries in `...` must match the type of `DEFAULT`.")
      }
      
      if (length(n_res) == 1L) {
        out[w] <- n_res
      } else if (length(n_res) == length(Expr)) {
        out[w] <- n_res[w]
      } else {
        stop("Argument number ", n, " named ", dot_noms[n], " had length ", 
             length(n_res), " but `length(Expr) = ", length(Expr), "`. ")
      }
    }
  }
  
  if (MUST_MATCH && min(matches) == 0L) {
    stop("Position ", which.min(matches),
         " uses the default value, so stopping, as required.")
  }
  
  
  out
}

