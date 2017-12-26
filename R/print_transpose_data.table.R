#' Print transposed data.table
#' @param DT A \code{data.table}.
#' @param sep Passed to \code{\link[base]{cat}}: the separation put horizontally between each row in \code{DT}.
#' @param file Passed to \code{\link[base]{cat}}. If \code{file = ""}, the default, 
#' output is printed to the console. Otherwise, result will be sunk to a file.
#' @param append Should the result be appended to \code{file}? Not applicable if \code{file = ""}.
#' @details \code{NA}s will be replaced by \code{.} and backslashes will be replaced by \code{@ } (to avoid backslashes).
#' @export

print_transpose_data.table <- function(DT, sep = "", file = "", append = FALSE) {
  max_nchar <- function(v) {
    v_na <- is.na(v)
    out <- as.character(v)
    out[v_na] <- ""
    max(nchar(encodeString(out), type = "width"))
  }
  
  char_width <- max(vapply(DT, max_nchar, integer(1)))
  max_width_names <- max(nchar(names(DT)))
  
  if (!append && nzchar(file) && file.exists(file)) {
    file.remove(file)
  }
  
  for (var in names(DT)) {
    # append = TRUE otherwise last cat will overwrite
    cat(formatC(var, width = max_width_names), " : ",
        sep = "", file = file, append = TRUE)
    v <- DT[[var]]
    v_na <- is.na(v)
    v <- as.character(v)
    v[v == "\\"] <- "@"
    v <- sub("\\", "@", v, fixed = TRUE)
    v[v_na] <- "."
    v <- formatC(v, width = char_width)
    # stop(var)
    cat(v, sep = sep, file = file, append = TRUE)
    cat("\n", sep = sep, file = file, append = TRUE)
  }
}
