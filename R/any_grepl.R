#' Does the pattern appear anywhere?
#' @description Shortcut for \code{any(grepl(...))}, mostly for consistency.
#' @param x A character vector.
#' @param pattern,perl,ignore.case,fixed As in \code{\link[base]{grep}}. 
#' @param quiet (logical, default: \code{FALSE}) If \code{TRUE}, silences any messages.
#' 
#' @examples
#' any_grepl(c("A_D_E", "K0j"), "[a-z]")
#' 
#' @export any_grepl
#' 

any_grepl <- function(x,
                      pattern,
                      perl = TRUE,
                      ignore.case = FALSE,
                      fixed = FALSE, 
                      quiet = FALSE) {
  if (fixed && ignore.case) {
    if (!quiet) {
      message("`fixed` and `ignore.case` both TRUE, ",
              "so considering both upper and lowercase ",
              "versions of pattern with fixed = TRUE.")
    }
    # For convenience
    return(any_grepl(tolower(x),
                     pattern = tolower(pattern), 
                     perl = FALSE,
                     ignore.case = FALSE,
                     fixed = TRUE))
  }
  
  if (missing(perl) && fixed) {
    perl <- FALSE
  }
  
  as.logical(length(grep(pattern = pattern,
                         x = x,
                         perl = perl,
                         fixed = fixed,
                         ignore.case = ignore.case)))
  
}



