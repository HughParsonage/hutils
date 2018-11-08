#' Weighted quantile
#' @description \code{quantile} when the values are weighted
#' @param v A vector from which sample quantiles are desired.
#' @param w Weights corresponding to each \code{v}.
#' @param p Numeric vector of probabilities. Missing values or values outside
#' \eqn{[0, 1]} raise an error.
#' @param v_is_sorted (logical, default: \code{FALSE}) If \code{TRUE}, ordering
#' \code{v} is assumed to be sorted. Only set to \code{TRUE} when it is certain
#' that \code{v} is sorted (as within groups of tables).
#' @return A vector the same length as \code{p}, the quantiles corresponding
#' to each element of \code{p}.
#' @export weighted_quantile

weighted_quantile <- function(v,
                              w = NULL,
                              p = (0:4)/4,
                              v_is_sorted = FALSE) {
  if (!length(p)) {
    stop("`p` had length zero. ",
         "Ensure `p` is a numeric vector with values in [0, 1].")
  }
  if (!is.numeric(p)) {
    stop("`p` was a ", class(p), " but must be numeric.")
  }
  if (anyNA(p)) {
    stop("`p` contained missing values. ",
         "Impute these values or remove the missing values.")
  }
  if (min(p) < 0) {
    stop("`p` contained negative values. ",
         "All values in `p` must be in [0, 1].")
  }
  if (max(p) > 1) {
    stop("`max(p) > 1`. ",
         "All values in `p` must be in [0, 1].")
  }
  
  if (length(w) <= 1L) {
    message("`w` is NULL or a single value, so returning unweighted quantiles.")
    return(stats::quantile(v, p, names = FALSE))
  }
  if (length(w) != length(v)) {
    stop("`length(v) = ", length(v), "`, yet ",
         "`length(w) = ", length(w), "`. ", 
         "The lengths of `v` must be `w` must be equal.")
  }
  if (v_is_sorted) {
    ov <- seq_along(v)
    P <- cumsum(w) / sum(w)
  } else {
    ov <- order(v)
    P <- cumsum(w[ov]) / sum(w)
  }
  
  iv <- integer(length(p))
  for (i in seq_along(iv)) {
    iv[i] <- which.max(P >= p[i])
  }
  {v[ov]}[iv]
}

