#' Find string pattern in (text) file
#' @param file_contents A perl-regular expression as a search query.
#' @param basedir The root of the directory tree in which files will be searched recursively.
#' @param dir_recursive (logical, default: \code{TRUE}) Search within subdirectories of \code{basedir}?
#' @param reader A function, akin to \code{base::readLines}, the default, that accepts a filename and returns a character vector.
#' @param include.comments If \code{FALSE}, the default, comments (i.e. anything after a \code{\#}) are not searched.
#' @param comment.char If \code{include.comments} is \code{FALSE}, what character marks a comment character? By default, \code{NULL}, which sets the correct comment symbol for R and TeX files. 
#' @param use.OS Use the operating system to determine file list. Only available on Windows. If it fails, a fall-back option
#' (using \code{dir}) is used.
#' @param file_pattern A regular expression passed to \code{list.files(pattern = file.ext)}.
#' By default, \code{"\\.(R|r)(nw|md)?$"}, i.e. all R and Sweave files. (Does not have to be a file extension.)
#' @param file_contents_perl (logical, default: \code{TRUE}) Should \code{file_contents} 
#' be interpreted as a \code{perl} regex? 
#' @param file_contents_fixed (logical, default: \code{FALSE}) Should \code{file_contents} 
#' be interpreted as a \code{fixed} regex?
#' @param file_contents_ignore_case (logical, default: \code{FALSE}) As in \code{\link[base]{grep}}.
#' @param file.ext A file extension passed to the operating system if \code{use.OS} is used.
#' @param which_lines One of \code{"first"} and \code{"all"}. If \code{"first"} only the first match in any file is returned in the result; if \code{"all"}, all matches are.
#' @return A \code{data.table}, showing the matches per file.
#' @details For convenience, if \code{file_contents} appears to be a directory
#' and \code{basedir} does not, the arguments are swapped, but with a warning.
#' @export

find_pattern_in <- function(file_contents,
                            basedir = ".",
                            dir_recursive = TRUE,
                            reader = readLines,
                            include.comments = FALSE,
                            comment.char = NULL,
                            use.OS = FALSE,
                            file_pattern = "\\.(R|r)(nw|md)?$",
                            file_contents_perl = TRUE,
                            file_contents_fixed = FALSE,
                            file_contents_ignore_case = FALSE,
                            file.ext = NULL, 
                            which_lines = c("first", "all")) {
  # Harmonize perl,fixed,ignore_case
  if (file_contents_fixed && missing(file_contents_perl)) {
    file_contents_perl <- FALSE
  }
  
  # Invert arguments if likely
  if (!dir.exists(basedir) && dir.exists(file_contents)) {
    warning("`basedir = ", basedir, "` and `file_contents = ", file_contents, "`. ",
            "Since `basedir` is not a directory but `file_contents` is a directory, ",
            "interpreting as the arguments reversed. ",
            "Do not rely on this behaviour as it may change without notice.")
    file_contents %<->% basedir
  }

  
  ..reader <- match.fun(reader)
  .reader <- function(x) {
    out <- ..reader(x)
    if (!include.comments) {
      if (is.null(comment.char)) {
        comment.char <-
          switch(tools::file_ext(x),
                 "R" = "[#]",
                 "Rmd" = "[#]",
                 "Rnw" = "[#%]",
                 "tex" = "%",
                 "#")
      }
      
      out <- sub(paste0(comment.char, ".*$"), "", out, perl = TRUE)
    }
    out
  }
  
  
  if (length(file.ext)) {
    stopifnot(length(file.ext) == 1L,
              is.character(file.ext))
    if (!grepl("^[*]?[.]?[0-9A-Za-z]+$", file.ext)) {
      stop("`file.ext` must be a string of alphanumeric characters, ",
           "optionally preceded by\n\t*\n or\n\t*.")
    }
  }
  
  

  
  
  if (use.OS) {
      if (missing(file_pattern)) {
        if (!is.null(file.ext)) {
          if (!file.exists("find--pattern.txt")) deprecate_windows_finder()
        }
      }
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
  
  which_lines <- match.arg(which_lines)
  matched_line_no <- function(x, .pattern = file_contents) {
    switch(which_lines,
           "first" = {
             grep(.pattern,
                  x = x,
                  perl = file_contents_perl,
                  ignore.case = file_contents_ignore_case,
                  fixed = file_contents_fixed)[1L]
           }, 
           "all" = {
             grep(.pattern,
                  x = x,
                  perl = file_contents_perl,
                  ignore.case = file_contents_ignore_case,
                  fixed = file_contents_fixed)
           })
  }
  
  matched_pattern <- function(x, .pattern = file_contents) {
    switch(which_lines,
           "first" = {
             grep(.pattern,
                  x = x,
                  value = TRUE,
                  perl = file_contents_perl,
                  ignore.case = file_contents_ignore_case,
                  fixed = file_contents_fixed)[1L]
           }, 
           "all" = {
             grep(.pattern,
                  x = x,
                  value = TRUE,
                  perl = file_contents_perl,
                  ignore.case = file_contents_ignore_case,
                  fixed = file_contents_fixed)
           })
  }
  
  has_pattern <- vapply(all_lines, contains_pattern, FALSE)
  if (any(has_pattern)) {
    switch(which_lines, 
           "first" = {
             first_line_nos <- vapply(all_lines[has_pattern], matched_line_no, integer(1L))
             first_lines <- vapply(all_lines[has_pattern], matched_pattern, character(1L))
             data.table(file = R_files[has_pattern],
                        line_no = first_line_nos,
                        lines = first_lines)
           }, 
           "all" = {
             lapply(which(has_pattern), function(d) {
               data.table(file = R_files[d], 
                          line_no = matched_line_no(all_lines[[d]]), 
                          lines = matched_pattern(all_lines[[d]]))
             }) %>%
               rbindlist
           })
    
  } else {
    data.table()
  }
}
  
#nocov start
deprecate_windows_finder <- function() {
  if (identical(.Platform$OS.type,
                "windows")) .Deprecated("hutilsInteractive::find_pattern_in_windows")
}
#nocov end




