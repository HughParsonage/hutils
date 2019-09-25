#' Provide a file
#' @description Present since \code{hutils v1.5.0}. 
#' @param path A string. The path to a filename that requires existence.
#' @param on_failure The return value on failure. By default, an empty string.
#' @return \code{path} for success. Or \code{on_failure} if the \code{path} cannot be provided.
#' @export provide.file

provide.file <- function(path, on_failure = "") {
  stopifnot(is.character(path),
            length(path) == 1L,
            nzchar(path))
  provide.dir(dirname(path))
  # If the file exists, OK; 
  # otherwise, create the file.
  if (file.exists(path) || file.create(path, showWarnings = FALSE)) {
    return(path)
  } else {
    # If the file creation failed, return the value of on_failure
    on_failure  # nocov
  }
}
