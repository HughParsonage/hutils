

print_transpose_data.table <- function(DT) {
  max_nchar <- function(v) {
    v_na <- is.na(v)
    out <- as.character(v)
    out[v_na] <- ""
    max(nchar(encodeString(out), type = "width"))
  }
  
  char_width <- max(vapply(DT, max_nchar, integer(1)))
  max_width_names <- max(nchar(names(DT)))
  
  for (var in names(DT)) {
    cat(formatC(var, width = max_width_names), ":")
    v <- DT[[var]]
    v_na <- is.na(v)
    v <- as.character(v)
    v[v == "\\"] <- "@"
    v[v_na] <- "."
    v <- formatC(v, width = char_width)
    # stop(var)
    cat(v, sep = "")
    cat("\n")
  }
}
