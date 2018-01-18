#' #' Logical implies
#' @description Returns the result of \eqn{x\Longrightarrow y}.
#' @param x,y Logical vectors of the same length.
#' @return \code{TRUE} along \code{y} unless \code{x} is \code{TRUE} and \code{y} is \code{FALSE}.
#' @export %implies% implies
#' 
#' @examples  
#' library(data.table)
#' CJ(x = c(TRUE,
#'          FALSE),
#'    y = c(TRUE,
#'          FALSE))[, ` x => y` := x %implies% y][]
#'
#' #>        x     y  x => y
#' #> 1: FALSE FALSE    TRUE
#' #> 2: FALSE  TRUE    TRUE
#' #> 3:  TRUE FALSE   FALSE
#' #> 4:  TRUE  TRUE    TRUE
#' 
#' # NA results:
#' #> 5:    NA    NA      NA
#' #> 6:    NA FALSE      NA
#' #> 7:    NA  TRUE    TRUE
#' #> 8: FALSE    NA    TRUE
#' #> 9:  TRUE    NA      NA


implies <- function(x, y) {
  lx <- length(x)
  ly <- length(y)
  
  if (lx != ly && NOR(lx == 1L, ly == 1L)) {
    dx <- deparse(substitute(x))
    dy <- deparse(substitute(y))
    stop('`', dx, "` and `", dy, "` had unequal lengths ",
         "(length(x) = ", lx, "; ",
         "length(y) = ", ly, ").",
         "\n",
         "`x` and `y` must be the same length.")
  }
  
  x <- as.logical(x)
  y <- as.logical(y)
  
  out <- y
  out[!coalesce(x, y)] <- TRUE
  if (anyNA(y)) {
    out[is.na(x) & coalesce(!y, TRUE)] <- NA
  }
  out
}

#' @rdname implies
`%implies%` <- implies


