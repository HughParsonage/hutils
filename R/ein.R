#' Exists and (not) in
#' @rdname ein
#' @description A common blunder in R programming is to mistype one of a set of filters without realizing. 
#' This function will error if any member of the values to be matched against is not present.
#' 
#' @param x Values to be matched
#' @param y Values to be matched against.
#' 
#' @examples 
#' # Incorrectly assumed to include two Species
#' iris[iris$Species %in% c("setosa", "versicolour"), ]
#' \dontrun{
#' # Error:
#' iris[iris$Species %ein% c("setosa", "versicolour"), ]
#' }
#' @return Same as \code{\%in\%} and \code{\%notin\%}, unless an element of \code{y} is not present in \code{x}.
#' @export

`%ein%` <- function(x, y) {
  if (!all(y %fin% x)) {
    badys <- y[y %notin% x]
    stop("Not all y are in x, so stopping, as requested. First absent y: ", badys[1])
  }
  x %fin% y
}

#' @rdname ein
#' @export 
`%enotin%` <- function(x, y) {
  if (!all(y %fin% x)) {
    badys <- y[y %notin% x]
    stop("Not all y are in x, so stopping, as requested. First absent y: ", badys[1])
  }
  x %notin% y
}
