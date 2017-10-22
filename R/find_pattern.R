#' Find string pattern in (text) file
#' @param file_contents A perl-regular expression as a search query.
#' @param basedir The root of the directory tree in which files will be searched recursively.
#' @param reader A function, akin to \code{base::readLines}, the default, that accepts a filename and returns a character vector.
#' @param include.comments If \code{FALSE}, the default, comments (i.e. anything after a \code{\#}) are not searched.
#' @param file.ext A regular expression passed to \code{list.files(pattern = file.ext)}. 
#' By default, \code{"\\.(R|r)(nw|md)?$"}, i.e. all R and Sweave files.
#' @return A \code{data.table}, one row per filename with a match, including the first line that matched.
#' @export


find_pattern_in <- function(file_contents,
                            basedir = ".",
                            reader = readLines,
                            include.comments = FALSE,
                            file.ext = "\\.(R|r)(nw|md)?$") {
  .reader <- match.fun(reader)
  
  if (toupper(.Platform$OS.type) == "WINDOWS" && 
      !file.exists("find--pattern.txt")) {
    current_wd <- getwd()
    setwd(basedir)
    tryCatch(shell("dir /b /s *.R > find--pattern.txt"),
             error = function(e) {
               setwd(current_wd)
               stop(e)
             })
    
    R_files <- reader("find--pattern.txt")
    invisible(file.remove("find--pattern.txt"))
    setwd(current_wd)
  } else {
    
    R_files <- 
      list.files(path = basedir,
                 pattern = "\\.R(nw)?$",
                 full.names = TRUE,
                 recursive = TRUE)
  }
  
  all_lines <- lapply(R_files, .reader)
  
  has_pattern <- vapply(all_lines, function(x) any(grepl(file_contents, x, perl = TRUE)), FALSE)
  if (any(has_pattern)) {
    first_line_no <- vapply(all_lines[has_pattern], function(x) grep(file_contents, x, perl = TRUE)[1], integer(1))
    lines <- vapply(all_lines[has_pattern], function(x) grep(file_contents, x, value = TRUE)[1], character(1))
    data.table(file = R_files[has_pattern],
               line_no = first_line_no,
               lines = lines)
  } else {
    data.table()
  }
}
