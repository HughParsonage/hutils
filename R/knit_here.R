#' Knit here
#' @param path
#' 
#' 

knit_here <- function(path = ".") {
  if (requireNamespace("knitr", quietly = TRUE) && identical(path, ".")) {
    the_Rnw <- dir(pattern = "\\.Rnw$")[1L]
    knitr::knit(the_Rnw)
  }
}

