#' Safer sampler
#' @description Present since \code{hutils v1.4.0}. 
#' Same as \code{\link[base]{sample}}, but avoiding the behaviour when 
#' \code{length(x) == 1L}.
#' @param x A vector.
#' @param size A non-negative integer, the number of items to return.
#' @param replace Should the sampling be done with replacement? Defaults to \code{TRUE} if 
#' \code{size > length(x)}, with a message.
#' @param loud If \code{TRUE}, the default, any behaviour known to be different from 
#' \code{\link[base]{sample}} is flagged with a message.
#' @param prob As in \code{\link[base]{sample}}.
#' 
#' @examples
#' samp(1:5)
#' sample(1:5)
#' 
#' samp(1:5, size = 10)  # no error
#' tryCatch(sample(1:5, size = 10), 
#'          error = function(e) print(e$m))
#' 
#' samp(5, size = 3)
#' sample(5, size = 3)
#' 
#' 
#' @export
#' 

samp <- function(x,
                 size = length(x),
                 replace = size > length(x),
                 loud = TRUE,
                 prob = NULL) {
  if (length(x) == 0L) {
    if (loud) {
      message("`length(x) == 0`, so returning `x`")
    }
    return(x)
  }
  if (!is.integer(size) || length(size) != 1L || is.na(size) || size < 0L) {
    if (!is.numeric(size)) {
      stop("`size` was a ", paste0(class(size), collapse = " "), ", but must be an integer. ",
           "Ensure `size` is a non-negative integer.")
    }
    if (length(size) != 1L) {
      stop("`size` had length ", length(size), ", but must be length-one. ",
           "Ensure `size` is a non-negative integer.")
    }
    if (is.na(size)) {
      stop("`size = NA`, but this is not permitted. ", 
           "Ensure `size` is a non-negative integer.")
    }
    sizei <- as.integer(size)
    
    if (sizei != size) {
      stop("`size = was type double and could not be safely coerced to integer. ",
           "Ensure `size` is a non-negative integer.")
    }
    if (size < 0L) {
      stop("`size < 0`, but this is not permitted. ", 
           "Ensure `size` is a non-negative integer.")
    }
  }
  if (loud && missing(replace) && replace) {
    message("Using `replace = ", replace, "`.")
  }
  if (length(x) == 1L) {
    if (loud) {
      message("`length(x) = 1`, so returning `rep.int(x, ", size, ")`.")
    }
    return(rep.int(x, size))
  }
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

# Don't -- not yet worth it
# samp.data.table <- function(DT, size, replace = nrow(DT) < size) {
#   if (length(size) != 1L) {
#     stop("wrong size value")
#   }
#   if (size == 1) {
#     warning("size = 1.0, interpreting as 100% sample. Use `size = 1L` for a random row.")
#     return(DT)
#   }
#   if (size < 1) {
#     rows <- sample.int(nrow(DT), size = nrow(DT) * size, replace = replace)
#   } else {
#     rows <- sample.int(nrow(DT), size = size, replace = replace)
#   }
#   DT[rows]
# }
