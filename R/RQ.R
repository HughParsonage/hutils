#' Shorthand for \code{requireNamespace}
#' @description \code{if (!requireNamespace(pkg, quietly = TRUE))} \emph{yes} \code{else} \emph{no}.
#' Typical use-case would be \code{RQ(pkg, install.packages("pkg"))].}
#' 
#' This function is not recommended for use in scripts as it is a bit cryptic; it's 
#' use-case is for bash scripts and the like where calls like this would otherwise
#' be frequent and cloud the message.
#' 
#' 
#' @param pkg Packages to test whether the package is not yet installed.
#' @param yes Response if \code{pkg} is \strong{not} installed.
#' @param no Response if \code{pkg} is installed.
#' @export
#' 
#' @examples
#' 
#' 
#' 

RQ <- function(pkg, yes, no) {
  .pkg <- as.character(substitute(pkg))
  stopifnot(length(.pkg) == 1L)
  if (!requireNamespace(.pkg, quietly = TRUE)) {
    yes
  } else if (!missing(no)) {
    no
  }
}


