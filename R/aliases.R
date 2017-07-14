#' @title Aliases
#' @name aliases
#' @param x,y Logical conditions.
NULL

#' @rdname aliases
#' @export
AND <- `&&`

#' @rdname aliases
#' @export
OR <- `||`

#' @rdname aliases
#' @export neither nor
neither <- nor <- function(x, y) {
  `&`(!x, !y)
}

#' @rdname aliases
#' @export NEITHER NOR
NEITHER <- NOR <- function(x, y) {
  !x && !y
}
  