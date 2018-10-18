#' @title hutils package
#' @name hutils-package
#' @description Provides utility functions for, and drawing on, the 'data.table' package. The package also collates useful miscellaneous functions extending base R not available elsewhere. The name is a portmanteau of 'utils' and the author.
#' @details The package attempts to provide lightweight, fast, and stable functions 
#' for common operations. 
#' 
#' By \strong{lightweight}, I mean in terms of dependencies:
#' we import \code{package:data.table} and \code{package:fastmatch} which do require
#' compilation, but in C. Otherwise, all dependencies do not require compilation.
#' 
#' By \strong{fast}, I mean essentially as fast as possible without using compilation.
#' 
#' By \strong{stable}, I mean that unit tests \emph{should not change} unless the major
#' version also changes.  To make this completely transparent, tests include the version
#' of their introduction and are guaranteed to not be modified (not even in the sense of
#' adding extra, independent tests) while the major version is \code{1}. Tests that do
#' not include the version in their filename may be modified from version to version 
#' (though this will be avoided).
#' 
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom magrittr not
#' @importFrom magrittr and
#' @importFrom magrittr is_greater_than
#' @importFrom fastmatch %fin%
#' @importFrom fastmatch fmatch
#' @importFrom grDevices dev.copy2pdf
#' @importFrom stats runif
#' @importFrom utils head
NULL
