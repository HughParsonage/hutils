#' Shorthand for \code{requireNamespace}
#' @description Present since \code{hutils v1.2.0}. Alias for \code{if (!requireNamespace(pkg, quietly = TRUE))} \emph{yes} \code{else} \emph{no}.
#' Typical use-case would be \code{RQ(pkg, install.packages("pkg"))].}
#' 
#' Default values for \code{yes} and \code{no} from \code{hutils v1.5.0}.
#' 
#' This function is not recommended for use in scripts as it is a bit cryptic; its 
#' use-case is for bash scripts and the like where calls like this would otherwise
#' be frequent and cloud the message.
#' 
#' 
#' @param pkg Package to test whether the package is not yet installed.
#' @param yes Response if \code{pkg} is \strong{not} installed.
#' @param no (optional) Response if \code{pkg} is installed.
#' @export
#' 
#' @examples
#' \dontrun{
#'  RQ("dplyr", "dplyr needs installing")
#' }
#' 
#' 

RQ <- function(pkg, yes = NULL, no = NULL) {
  .pkg <- as.character(substitute(pkg))
  stopifnot(length(.pkg) == 1L)
  if (!requireNamespace(.pkg, quietly = TRUE)) {
    yes
  } else {
    no
  }
}


