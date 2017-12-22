#' Quickly count the number of files in a directory


number_of_files <- function(dir, filetype = "*.*") {
  stopifnot(.Platform$OS.type == "windows")
  stopifnot(nzchar(Sys.which("powershell")))
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(dir)
  result <- system2("powershell",
                    args = c("-command", "Write-Host (dir . | measure).Count;"),
                    stdout = TRUE, 
                    stderr = "")
  # The following is faster via powershell but I can't invoke it
  # result <- system2("powershell", 
  #                   args = c("-command", '[System.IO.Directory]::GetFiles(".", "*.*").Count'))
  setwd(current_wd)
  result
}
