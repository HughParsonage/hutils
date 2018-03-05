#' Negation of in (character)
#' @param x Values to be matched.
#' @param y Values to be matched against.
#' @details If \code{y} is \code{NULL}, then \code{x} is \code{TRUE} for consistency with
#'  \code{\%in\%}. If \code{x} and \code{y} are not both character, the function simply
#'  falls back to \code{\%in\%} rather than erroring.
#' @export

`%notchin%` <- function(x, y) {
  if (is.null(y)) {
    rep_len(TRUE, length(x))
  } else {
    if (is.character(x) && is.character(y)) {
      is.na(chmatch(x, y))
    } else {
      !{x %in% y}
    }
  }
}
