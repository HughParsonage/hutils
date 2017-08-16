#' Anti-grep
#' @description It is not simple to negate a regular expression. This obviates the need 
#' takes the long way round: negating the corresponding \code{grepl} call.
#' @param x,value,pattern As in \code{\link[base]{grep}}.
#' @param ... Arguments passed to \code{grepl}.
#' @return If \code{value} is \code{FALSE} (the default), indices of \code{x} which do not match the 
#' pattern; if \code{TRUE}, the values of \code{x} themselves.
#' @examples 
#'  grep("[a-h]", letters)
#' ngrep("[a-h]", letters)
#' 
#' txt <- c("The", "licenses", "for", "most", "software", "are",
#' "designed", "to", "take", "away", "your", "freedom",
#' "to", "share", "and", "change", "it.",
#' "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
#' "is", "intended", "to", "guarantee", "your", "freedom", "to",
#' "share", "and", "change", "free", "software", "--",
#' "to", "make", "sure", "the", "software", "is",
#' "free", "for", "all", "its", "users")
#' 
#'  grep("[gu]", txt, value = TRUE)
#' ngrep("[gu]", txt, value = TRUE)
#' 
#' @export

ngrep <- function(pattern, x, value = FALSE, ...) {
  if (value) {
    x[!grepl(pattern = pattern, x = x, ...)]
  } else {
    which(!grepl(pattern = pattern, x = x, ...))
  }
}
