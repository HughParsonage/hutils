#' @title Aliases
#' @description These simple aliases can be useful to avoid operator precedence ambiguity,
#' or to make use of indents from commas within your text editor. The all-caps versions accept
#' single-length (capable of 'short-circuits') logical conditions only.
#' @name aliases
#' @param x,y Logical conditions.
NULL

#' @rdname aliases
#' @usage AND(x, y)
#' @export
AND <- `&&`

#' @rdname aliases
#' @usage OR(x, y)
#' @export
OR  <- `||`

#' @rdname aliases
#' @export
nor <- function(x, y) {
  `&`(!x, !y)
}

#' @rdname aliases
#' @export
neither <- function(x, y) nor(x, y)

#' @rdname aliases
#' @export 
NOR <- function(x, y) {
  !x && !y
}

#' @rdname aliases
#' @export 
NEITHER <- function(x, y) NOR(x, y)
