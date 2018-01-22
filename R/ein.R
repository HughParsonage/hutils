#' Exists and (not) in
#' @rdname ein
#' @description A common blunder in R programming is to mistype one of a set of filters without realizing. 
#' This function will error if any member of the values to be matched against is not present.
#' 
#' @param lhs Values to be matched
#' @param rhs Values to be matched against.
#' 
#' @examples 
#' # Incorrectly assumed to include two Species
#' iris[iris$Species %in% c("setosa", "versicolour"), ]
#' \dontrun{
#' # Error:
#' iris[iris$Species %ein% c("setosa", "versicolour"), ]
#' }
#' @return Same as \code{\%in\%} and \code{\%notin\%}, unless an element of \code{rhs} is not present in \code{lhs}, in which case, an error.
#' @export

`%ein%` <- function(lhs, rhs) {
  if (anyNA(fmatch(rhs, lhs))) {
    badrhss <- rhs[rhs %notin% lhs]
    lhs_deparsed <- deparse(substitute(lhs), control = NULL)
    
    report_error(faulty_input = rhs,
                 error_condition = paste0("contained ", badrhss[1], ", ",
                                          "but this value was not found in `lhs = ", lhs_deparsed, "`."),
                 requirement = "All values of `rhs` must be in `lhs`.",
                 advice = "Ensure you have specified `rhs` correctly.")
  }
  lhs %fin% rhs
}

#' @rdname ein
#' @export 
`%enotin%` <- function(lhs, rhs) {
  if (!all(rhs %fin% lhs)) {
    badrhss <- rhs[rhs %notin% lhs]
    lhs_deparsed <- deparse(substitute(lhs), control = NULL)
    report_error(faulty_input = rhs,
                 error_condition = paste0("contained ", badrhss[1], ", ",
                                          "but this value was not found in `lhs = ", lhs_deparsed, "`."),
                 requirement = "All values of `rhs` must be in `lhs`.",
                 advice = "Ensure you have specified `rhs` correctly.")
  }
  lhs %notin% rhs
}
