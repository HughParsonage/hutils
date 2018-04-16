#' Copy device to an A4 PDF
#' @description Simply a wrapper around \code{dev.copy2pdf}, but without the need to remember that an A4 sheet of paper is 8.27 in by 11.69 in.
#' @param filename A string giving the name of the PDF file to write to, must end in \code{.pdf}.
#' @param ... Other parameters passed to \code{\link[grDevices]{pdf}}.
#' @return As in \code{\link[grDevices]{dev2}}.
#' @export

dev_copy2a4 <- function(filename, ...) {
  if (missing(filename)) {
    stop("`filename` is missing, with no default.")
  }
  if (length(filename) != 1L) {
    stop("`filename` had length ", length(filename), ". ", 
         "`filename` must be a length-one character vector.")
  }
  if (!is.character(filename)) {
    stop("`filename` was type ", typeof(filename), ". ", 
         "`filename = ", deparse(substitute(filename)), "` must be a length-one character vector.")
  }
  if (!grepl("\\.pdf$", filename)) {
    stop('`filename = "', filename, '"` does not end with .pdf')
  }
  
  dev.copy2pdf(file = filename, width = 11.69, height = 8.27, ...)
}
