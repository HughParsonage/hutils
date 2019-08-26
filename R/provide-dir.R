#' Provide directory
#' @description Provide directory. Create directory only if it does not exist.
#' @param path Path to create.
#' @param ... Passed to \code{dir.create}.
#' @return \code{path} on success, the empty string \code{character(1)} on failure.
#' @export 

provide.dir <- function(path, ...) {
  if (dir.exists(path) || dir.create(path, recursive = TRUE, ...)) {
    return(path)
  }
  ""
}
