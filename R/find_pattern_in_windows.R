
find_pattern_if_windows <- function(file.ext, basedir, .reader) {
  current_wd <- getwd()
  setwd(basedir)
  
  if (file.create("find--pattern.txt", showWarnings = FALSE)) {
    shell_result <- 
      tryCatch(shell(paste0("dir /b /s *",
                            sub("*", "", file.ext, fixed = TRUE),
                            " > find--pattern.txt")),
               error = function(e) {
                 setwd(current_wd)
                 stop(e)
               })
    
    out <- list(shell_result = shell_result,
                R_files = .reader("find--pattern.txt"))
    invisible(file.remove("find--pattern.txt"))
    setwd(current_wd)
    out
  } else {
    setwd(current_wd)
    list(shell_result = -1,
         R_files = NULL)
  }
}