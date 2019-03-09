#' Replace string pattern in text file
#' @param file_contents Character string containing a regular expression to be matched in the 
#' given character vector. Passed to \code{pattern} in \code{\link[base]{gsub}}.
#' @param replace The replacement, passed to \code{replacement} in \code{\link[base]{gsub}}.
#' @param basedir The root of the directory tree in which files will be searched recursively.
#' @param dir_recursive (logical, default: \code{TRUE}) Search within subdirectories of \code{basedir}?
#' @param reader A function, akin to \code{base::readLines}, the default, that accepts a filename and returns a character vector.
#' @param file_pattern A regular expression passed to \code{list.files(pattern = file.ext)}.
#' By default, \code{"\\.(R|r)(nw|md)?$"}, i.e. all R and Sweave files. (Does not have to be a file extension.)
#' @param file_contents_perl (logical, default: \code{TRUE}) Should \code{file_contents} 
#' be interpreted as a \code{perl} regex? 
#' @param file_contents_fixed (logical, default: \code{FALSE}) Should \code{file_contents} 
#' be interpreted as a \code{fixed} regex?
#' @param file_contents_ignore_case (logical, default: \code{FALSE}) As in \code{\link[base]{grep}}.
#' @param writer A function that will rewrite the file from the character vector read in.
#' @export replace_pattern_in
replace_pattern_in <- function(file_contents,
                               replace, 
                               basedir = ".",
                               dir_recursive = TRUE,
                               reader = readLines,
                               file_pattern = "\\.(R|r)(nw|md)?$",
                               file_contents_perl = TRUE,
                               file_contents_fixed = FALSE,
                               file_contents_ignore_case = FALSE,
                               writer = writeLines) {
  if (file_contents_fixed && missing(file_contents_perl)) {
    file_contents_perl <- FALSE
  }
  .writer <- match.fun(writer)
  .reader <- match.fun(reader)
  R_files <-
    list.files(path = basedir,
               pattern = file_pattern,
               full.names = TRUE,
               recursive = dir_recursive)
  
  for (file.R in R_files) {
    lines_text <- .reader(file.R)
    out_lines <- gsub(file_contents,
                      replace,
                      lines_text, 
                      perl = file_contents_perl, 
                      fixed = file_contents_fixed,
                      ignore.case = file_contents_ignore_case)
    .writer(out_lines, file.R)
  }
}



