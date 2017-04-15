.onLoad <- function(libname = find.package("grattanReporter"), pkgname = "grattanReporter"){
  
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(c("."))
}
