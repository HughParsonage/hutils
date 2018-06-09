#' @title Aliases
#' @description These simple aliases can be useful to avoid operator precedence ambiguity,
#' or to make use of indents from commas within your text editor. The all-caps versions accept
#' single-length (capable of 'short-circuits') logical conditions only.
#' 
#' Neithers and nors are identical except have slightly different short-circuits.
#'  \code{NOR} uses negation once so may be quicker if the first argument is very, very prompt.
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
  not(x | y)
}

#' @rdname aliases
#' @export
neither <- function(x, y) !x & !y

#' @rdname aliases
#' @export 
NOR <- function(x, y) {
  not(x || y)
}

#' @rdname aliases
#' @export 
NEITHER <- function(x, y) !x && !y

#' @rdname aliases
#' @export
pow <- `^`

#' @rdname aliases
#' @export
XOR <- function(x, y) if (anyNA(x)) NA else if (x) !y else y
# speed: anyNA > is.na > is.na || is.na


