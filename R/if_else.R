#' Vectorized if
#' @description Lightweight \code{dplyr::if_else} with the virtues and vices that come from such an approach.
#' Attempts to replicate \code{dplyr::if_else} but written in base R for faster compile time.
#' \code{hutils::if_else} should be faster than \code{dplyr::if_else} \dots{} when it works, 
#' but will not work on lists or on factors. 
#' Additional attributes may be dropped.
#' @param condition Logical vector
#' @param true,false Where condition is \code{TRUE}/\code{FALSE}, use the corresponding\code{true}/\code{no} value.
#' Must have the same type as each other and be the same length as \code{condition} or length-one.
#' @param missing If condition is \code{NA}, use the corresponding \code{na} value. Like\code{true} and\code{false}, must
#' be of the same type and have the same length as condition, unless it has length one.
#' @return Where \code{condition} is \code{TRUE}, the corresponding value in \code{true}; 
#' where \code{condition} is \code{FALSE}, the corresponding value in \code{false}.
#' Where \code{condition} is \code{NA}, then the corresponding value in \code{na} -- 
#' unless \code{na} is \code{NULL} (the default) in which case the value will be \code{NA} (with the same
#' type as \code{true}.)
#' @source Original code but obviously heavily inspired by \url{https://CRAN.R-project.org/package=dplyr}. 
#' @export 


if_else <- function(condition, true, false, missing = NULL) {
  na <- missing
  yes <- true
  no <- false
  na_not_used <- is.null(na)

  max.length <- length(condition)
  lengths <- c(max.length, length(yes), length(no), if (na_not_used) 1L else length(na))
  if (any(lengths != 1L & lengths != max.length)) {
    if (length(yes) != 1L && length(yes) != max.length) {
      stop("`true` had length ", length(yes), " but `condition` had length ", length(condition), ". ", 
           "`true` must have the same length as `condition`, or length one.")
    }
    if (length(no) != 1L && length(no) != max.length) {
      stop("`false` had length ", length(no), " but `condition` had length ", length(condition), ". ", 
           "`false` must have the same length as `condition`, or length one.")
    }
    if (!na_not_used && length(missing) != 1L && length(missing) != max.length) {
      stop("`missing` had length ", length(missing), " but `condition` had length ", length(condition), ". ", 
           "`missing`, if not NULL, must have the same length as `condition`, or length one.")
    }
  }
  
  if (!is.logical(condition)) {
    if (typeof(condition) == "integer") {
      stop("`condition` must be a logical vector, but is currently an integer vector.")
    } else {
      stop("`condition` must be a logical, but is currently a ", typeof(condition), " vector")
    }
  }

  Type <- typeof(yes)
  
  if (!(Type %chin% c("logical", "integer", "double", "character"))) {
    stop("typeof(true) == ", Type, ". The only permitted types are logical, integer, double, and character.")
  }
  
  if (na_not_used) {
    if (typeof(no) != Type) {
      stop("typeof(false) == ", typeof(no), ", yet typeof(true) == ", Type, ". ", 
           "Both true and false must have the same type.")
    }
  } else {
    if (typeof(no) != Type) {
      stop("typeof(true) == ", typeof(no), ", yet typeof(false) == ", Type, ". ", 
           "All of true, false, and missing must have the same type.")
    }
    
    if (typeof(na) != Type) {
      stop("typeof(missing) == ", typeof(no), "yet typeof(true) == ", Type, ". ", 
           "All of true, false, and missing must have the same type.")
    }
  }

  if (lengths[3] == 1L) {
    out <- rep_len(no, max.length)
  } else {
    out <- no
  }

  NA_Type_ <- switch(Type,
                     "logical" = NA,
                     "integer" = NA_integer_,
                     "double" = NA_real_,
                     "character" = NA_character_)
  
  
  
  if (max.length == 1L) {
    if (is.na(condition)) {
      if (na_not_used) {
        out <- NA_Type_
      } else {
        out <- na
      }
    } else {
      if (condition) {
        out <- yes
      } else {
        out <- no
      }
    }
  } else {
    # N 1 ? ?
    if (anyNA(condition)) {
      Yes <- which(condition)
      
      if (lengths[2] == 1L) {
        out[Yes] <- yes
        if (na_not_used) {
          out[is.na(condition)] <- NA_Type_
        } else {
          if (lengths[4] == 1L) {
            out[is.na(condition)] <- na
          } else {
            out[is.na(condition)] <- na[is.na(condition)]
          }
        }
        
      } else {
        # N N ? ?
        out[Yes] <- yes[Yes]
        
        if (na_not_used) {
          out[is.na(condition)] <- NA_Type_
        } else {
          if (lengths[4] == 1L) {
            out[is.na(condition)] <- na
          } else {
            out[is.na(condition)] <- na[is.na(condition)]
          }
        }
      }
    } else {
      # No NAs to deal with
      if (lengths[2] == 1L) {
        out[condition] <- yes
      } else {
        out[condition] <- yes[condition]
      }
      
    }
  }
  
  out
}

  
  
  

