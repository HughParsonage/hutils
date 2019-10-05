
.anyNA2 <- function(x) {
  if (length(x) < 1e4L || !is.symbol(substitute(x))) {
    return(anyNA(x))
  } else if (is.null(anyNA2_attr <- attr(x, "hutils_anyNA"))) {
    out <- anyNA(x)
    setattr(x,
            name = "hutils_anyNA",
            value = list("ans" = out,
                         "address" = address(x)))
  } else if ({addressx <- address(x)} != .subset2(anyNA2_attr, "address")) {
    out <- anyNA(x)
    setattr(x,
            name = "hutils_anyNA",
            value = list("ans" = out,
                         "address" = addressx))
  } else {
    out <- .subset2(anyNA2_attr, "ans")
  }
  out
}


