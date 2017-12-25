#' Generate LaTeX manual of installed package
#' @param pkg Quoted package name (must be installed).
#' @return See \code{\link[base]{system}}.
#' Called for its side-effect: creates a PDF in the current working directory. Requires a TeX distribution.
#' @source \url{https://stackoverflow.com/a/30608000/1664978}
#' @export

generate_LaTeX_manual <- function(pkg) {
  pack <- pkg
  path <- find.package(pack)
  system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf", shQuote(path)))
}
