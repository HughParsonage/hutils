#' Provide directory
#' @description Provide directory. Create directory only if it does not exist.
#' @param path Path to create.
#' @param ... Passed to \code{dir.create}.
#' @export 

provide.dir <- function(path, ...) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, ...)
  }
}
