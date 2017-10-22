#' Partial in
#' @description Analogue of \code{\%in\%} but indicating partial match of the left operand.
#' @param x The values to be matched. Same as \code{\%in\%}.
#' @param Y A vector of values (perl regular expressions) to be matched against.
#' @return \code{TRUE} for every \code{x} for which any \code{grepl} is \code{TRUE}.
#' 
#' @examples 
#' x <- c("Sydney Airport", "Melbourne Airport")
#' 
#' x %pin% c("Syd", "Melb")
#' 
#' 
#' 
#' @export 

`%pin%` <- function(x, Y) {
  y <- sprintf("(?:%s)", paste0(Y, collapse = ")|(?:"))
  grepl(y, x, perl = TRUE)
}
