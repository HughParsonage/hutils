#' Negation of in
#' @param x Values to be matched
#' @param y Values to be matched against.
#' @export 

`%notin%` <- function(x, y){
  !(x %fin% y)
}
