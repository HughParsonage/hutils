if_else <- function(condition, yes, no, na = NULL) {
  na_not_used <- is.null(na)

  lengths <- c(length(condition), length(yes), length(no), if (na_not_used) 1L else length(na))
  max.length <- max(lengths)
  if (any(lengths != 1L & lengths != max.length)) {
    stop("Only permissible vector lengths are 1 or the maximum of the inputs.")
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
    stop("typeof(yes) == ", Type, ". The only permitted types are logical, integer, double, and character.")
  }
  
  if (na_not_used) {
    if (typeof(no) != Type) {
      stop("typeof(no) == ", typeof(no), ", yet typeof(yes) == ", Type, ". ", 
           "Both yes and no must have the same type.")
    }
  } else {
    if (typeof(no) != Type) {
      stop("typeof(no) == ", typeof(no), ", yet typeof(yes) == ", Type, ". ", 
           "All of yes, no, and na must have the same type.")
    }
    
    if (typeof(na) != Type) {
      stop("typeof(na) == ", typeof(no), "yet typeof(yes) == ", Type, ". ", 
           "All of yes, no, and na must have the same type.")
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
  
  
  
  if (lengths[1] == 1L) {
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
      Yes <- condition & !is.na(condition)
      
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

  
  
  

