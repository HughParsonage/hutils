#' Print data like print.data.table
#' @description I like the \code{print} method of \code{data.table}s. This is a simple
#' function to replicate it using \code{\link[knitr]{kable}}.
#' @param DT A \code{data.frame} or matrix coercible to such.
#' @param ... Arguments passed to \code{\link[knitr]{kable}}. (\code{format.args} is used here.)
#' @export

data.kable <- function(DT, ...) {
  current_knitr.kable.NA <- options("knitr.kable.NA")
  options(knitr.kable.NA = '...')
  on.exit(options(knitr.kable.NA = current_knitr.kable.NA))
  if (nrow(DT) > 50) {
    middle_row <- as.data.table(matrix(nrow = 1, ncol = ncol(DT)))
    setnames(middle_row, seq_along(middle_row), names(DT))
    DT_topn <- rbind(utils::head(DT),
                     middle_row,
                     utils::tail(DT))
    knitr::kable(DT_topn, format.args = list(big.mark = ","), ...)
  } else {
    knitr::kable(DT, format.args = list(big.mark = ","), ...)
  }
}