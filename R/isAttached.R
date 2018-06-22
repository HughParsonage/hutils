#' Is a package attached?
#' 
#' @param pkg Either character or unquoted. 
#' @return \code{TRUE} if \code{pkg} is attached.
#' @export

isAttached <- function(pkg) {
  .pkg <- as.character(substitute(pkg))
  .pkg %in% .packages()
}
