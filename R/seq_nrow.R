#' Generate sequence of row numbers
#' @param x An object that admits an \code{nrow}.
#' 
#' @return Equivalent to \code{seq_len(nrow(x))}
#' @export

seq_nrow <- function(x) seq_len(nrow(x)) # nocov
