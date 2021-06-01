#' Statistical mode 
#' @description Present since \code{hutils 1.4.0}. The most common element.
#' @param x A vector for which the mode is desired.
#' @return The most common element of \code{x}.
#' 
#' If the mode is not unique, only one of these values is returned, for simplicity.
#' 
#' If \code{x} has length zero, \code{Mode(x) = x}.
#' 
#' @export Mode

Mode <- function(x) {
  if (!length(x)) {
    return(x)
  }
  if (is.logical(x)) {
    if (anyNA(x)) {
      nas <- sum(is.na(x))
      yes <- sum(x, na.rm = TRUE)
      return(c(TRUE, FALSE, NA)[which.max(c(yes, length(x) - yes - nas, nas))])
    } else {
      if (2 * sum(x) > length(x)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
  if (is.integer(x)) {
    xmin <- min(x)
    if (is.na(xmin)) {
      ux <- unique(x)
    } else {
      xmax <- max(x)
      
      # Avoid creating very large indices
      # xmax/2 because xmax - xmin may overflow
      if ((xmax/2 - xmin/2) > length(x)) {
        ux <- unique(x)
      } else {
        ux <- xmin:xmax
      }
    }
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  ux <- unique(x)
  if (is.double(x) || is.character(x)) {
    return(ux[which.max(tabulate(fmatch(x, ux)))])
  } else {
    return(ux[which.max(tabulate( match(x, ux)))])
  }
}
