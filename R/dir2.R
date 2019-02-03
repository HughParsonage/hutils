#' List many files
#' 
#' @param path A string representing the trunk path to search within.
#' @param file_ext A string like `*.txt` or `.csv` to limit the result to files 
#' with that extension.
#' @param full.names \code{TRUE} by default.
#' @param recursive \code{TRUE} by default.
#' @param perl,ignore.case,fixed,invert As in \code{grep} but with different defaults.
#' @param .dont_use Only used for tests to simulate non-Windows systems.
#' 
#' 


dir2 <- function(path = ".", 
                 file_ext = NULL,
                 full.names = TRUE,
                 recursive = TRUE,
                 pattern = NULL,
                 fixed = FALSE,
                 perl = TRUE && missing(fixed) && !fixed,
                 ignore.case = FALSE,
                 invert = FALSE,
                 .dont_use = FALSE) {
  if (!identical(.Platform$OS.type, "windows") || .dont_use) {
    stop("Only useful on Windows.")
  }
  
  if (!identical(path, ".")) {
    if (!is.character(path)) {
      stop("`path` was a ", paste(class(path), collapse = " "),
           ", but must be a string identifying an extant directory.")
    }
    if (length(path) != 1L) {
      stop("`path` was length ", length(path), ", but must be length-one.")
    }
    if (!dir.exists(path)) {
      stop("`path = ", path,
           "` does not exist.")
    }
    old <- setwd(dir = path)
    on.exit(setwd(old))
  }
  temp.txt <- tempfile("dir2")
  
  stopifnot(is.character(file_ext),
            length(file_ext) == 1L,
            startsWith(file_ext, "*.") || startsWith(file_ext, "."))
  
  if (startsWith(file_ext, ".")) {
    file_ext <- paste0("*", file_ext)
  }
  
  
  shell(paste("dir /b ",
              if (is.null(file_ext)) "*.*" else file_ext,
              if (recursive) {
                "/S"
              }, 
              ">",
              temp.txt))
  out <- fread(temp.txt, sep = NULL, header = FALSE)
  if (length(out)) {
    out <- out[[1L]]
  } else {
    out <- character(0L)  # null.data.table
  }
  file.remove(temp.txt)
  if (!is.null(pattern)) {
    check_TF(perl)
    check_TF(ignore.case)
    check_TF(fixed)
    
    # Else fixed and perl warning when both TRUE
    if (is.null(perl) && missing(fixed)) {
      perl <- TRUE
    }
    
    out <- grep(pattern, 
                x = out,
                value = TRUE,
                perl = perl,
                fixed = fixed, 
                ignore.case = ignore.case, 
                invert = invert)
  }
  out
}



