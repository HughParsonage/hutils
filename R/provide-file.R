#' Provide a file
#' @param path A string. The path to a filename that requires existence.
#' @return \code{path} for success. Or the empty string \code{character(1)} if failures.
#' @export provide.file

provide.file <- function(path) {
  stopifnot(is.character(path),
            length(path) == 1L,
            nzchar(path))
  provide.dir(dirname(path))
  # If the file exists, OK; 
  # otherwise, create the file.
  if (file.exists(path) || file.create(path)) {
    return(path)
  } else {
    # If the file creation failed, return ""
    return("")
  }
}
