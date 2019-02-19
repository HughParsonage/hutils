.onLoad <- function(libname = find.package("grattanReporter"), pkgname = "grattanReporter"){
  backports::import(pkgname)
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(c("."))
}
