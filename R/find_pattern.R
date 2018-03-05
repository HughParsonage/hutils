#' Find string pattern in (text) file
#' @param file_contents A perl-regular expression as a search query.
#' @param basedir The root of the directory tree in which files will be searched recursively.
#' @param dir_recursive (logical, default: \code{TRUE}) Search within subdirectories of \code{basedir}?
#' @param reader A function, akin to \code{base::readLines}, the default, that accepts a filename and returns a character vector.
#' @param include.comments If \code{FALSE}, the default, comments (i.e. anything after a \code{\#}) are not searched.
#' @param use.OS Use the operating system to determine file list. Only available on Windows. If it fails, a fall-back option
#' (using \code{dir}) is used.
#' @param file_pattern A regular expression passed to \code{list.files(pattern = file.ext)}.
#' By default, \code{"\\.(R|r)(nw|md)?$"}, i.e. all R and Sweave files. (Does not have to be a file extension.)
#' @param file_contents_perl (logical, default: \code{TRUE}) Should \code{file_contents} 
#' be interpreted as a \code{perl} regex? 
#' @param file_contents_fixed (logical, default: \code{FALSE}) Should \code{file_contents} 
#' be interpreted as a \code{fixed} regex?
#' @param file.ext A file extension passed to the operating system if \code{use.OS} is used.
#' @return A \code{data.table}, one row per filename with a match, which includes the first line that matched.
#' @export

find_pattern_in <- function(file_contents,
                            basedir = ".",
                            dir_recursive = TRUE,
                            reader = readLines,
                            include.comments = FALSE,
                            use.OS = FALSE,
                            file_pattern = "\\.(R|r)(nw|md)?$",
                            file_contents_perl = TRUE,
                            file_contents_fixed = FALSE,
                            file.ext = NULL) {
  .reader <- match.fun(reader)
  if (length(file.ext)) {
    stopifnot(length(file.ext) == 1L,
              is.character(file.ext))
    if (!grepl("^[*]?[.]?[0-9A-Za-z]+$", file.ext)) {
      stop("`file.ext` must be a string of alphanumeric characters, ",
           "optionally preceded by\n\t*\n or\n\t*.")
    }
  }
  
  

  if (use.OS &&
      missing(file_pattern) &&
      !is.null(file.ext) &&
      !file.exists("find--pattern.txt")) {
    if (identical(.Platform$OS.type,
                  "windows")) .Deprecated("find_pattern_in_windows", package = "hutilsInteractive")
  }
  
  if (!is.null(file.ext)) {
    if (missing(file_pattern)) {
      file_pattern <-
        switch(substr(file.ext, 1L, 1L),
               "*" = {
                 file_pattern <- utils::glob2rx(file.ext)
               },
               "." = {
                 file_pattern <- utils::glob2rx(paste0("*", file.ext))
               },
               {
                 file_pattern <- file.ext
               })
    }
  }
  
  R_files <-
    list.files(path = basedir,
               pattern = file_pattern,
               full.names = TRUE,
               recursive = dir_recursive)
  
  all_lines <- lapply(R_files, .reader)
  
  contains_pattern <- function(x, .pattern = file_contents) {
    any(grepl(.pattern,
              x = x,
              perl = file_contents_perl,
              fixed = file_contents_fixed))
  }
  
  matched_line_no <- function(x, .pattern = file_contents) {
    grep(.pattern,
         x = x,
         perl = file_contents_perl,
         fixed = file_contents_fixed)[1L]
  }
  
  matched_pattern <- function(x, .pattern = file_contents) {
    grep(.pattern,
         x = x,
         value = TRUE,
         perl = file_contents_perl,
         fixed = file_contents_fixed)[1L]
  }
  
  has_pattern <- vapply(all_lines, contains_pattern, FALSE)
  if (any(has_pattern)) {
    first_line_nos <- vapply(all_lines[has_pattern], matched_line_no, integer(1L))
    first_lines <- vapply(all_lines[has_pattern], matched_pattern, character(1L))
    data.table(file = R_files[has_pattern],
               line_no = first_line_nos,
               lines = first_lines)
  } else {
    data.table()
  }
}
