#' Copy device to an A4 PDF
#' @description Simply a wrapper around \code{dev.copy2pdf}, but without the need to remember that an A4 sheet of paper is 8.27 in by 11.69 in.
#' @param filename A string giving the name of the PDF file to write to, must end in \code{.pdf}.
#' @param ... Other parameters passed to \code{\link[grDevices]{pdf}}.
#' @return As in \code{\link[grDevices]{dev2}}.
#' @export

dev_copy2a4 <- function(filename, landscape = TRUE, ...) {
  if (missing(filename)) {
    stop("`file` is missing, with no default.")
  }
  if (!endsWith(filename, ".pdf")) {
    stop("`file` does not end with .pdf")
  }
  
  dev.copy2pdf(file = file, width = 11.69, height = 8.27, ...)
}
