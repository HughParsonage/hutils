#' Maximum area given x and y coordinates
#' @param x,y Coordinates of a curve containing a rectangle. The 
#' 
#' @return The coordinates of a rectangle, from (0, 0), (1, 0), (1, 1), (0, 1), south-west clockwise, 
#' that is contained within the area of the chart for positive values only.
#' 
#' @examples ahull(c(0, 1, 2, 1, 0), c(0, 1, 2, 0, 0))
#' 
#' @export



ahull <- function(x, y) {
  dt <- data.table(x, y)
  setkey(dt, x)
  set_local_extrema(dt)
  extrema <- dt[(local_min)]
  extrema <- extrema[y >= 0]
  stopifnot(nrow(extrema) > 0)
  dt[y <= 0, local_min := FALSE]
  
  
  area_from_min <- function(ii) {
    stopifnot(is.integer(ii), 
              length(ii) == 1L,
              between(ii, 1, nrow(dt), incbounds = FALSE))
    x <- .subset2(dt, "x")
    y <- .subset2(dt, "y")
    n <- length(x)
    H <- y[ii]
    x_1 <- x[1]
    for (i in seq.int(ii - 1L, 2L)) {
      if (i > 1L && y[i - 1L] < H && y[i] > H) {
        x_02 <- x[i]
        x_01 <- x[i - 1L]
        y_02 <- y[i]
        y_01 <- y[i - 1L]
        XX <- x_01 + (H - y_01) * (x_02 - x_01) / (y_02 - y_01)
        x_1 <- XX
        break
      }
    }
    x_2 <- x[n]
    for (i in seq.int(ii + 1L, n - 1L)) {
      if (y[i - 1L] > H && y[i] < H) {
        x_02 <- x[i]
        x_01 <- x[i - 1L]
        y_01 <- y[i]
        y_02 <- y[i - 1L]
        
        XX <- x_02 - (H - y_01) * (x_02 - x_01) / (y_02 - y_01)
        x_2 <- XX
        break
      }
    }
    list(ii = ii,
         h = H,
         xmin = x_1, 
         xmax = x_2,
         area = y[ii] * {x_2 - x_1})
  }
  
  area_from_minima <- 
    rbindlist(lapply(which(dt[["local_min"]]), area_from_min))
  area_from_minima
}

set_local_extrema <- function(dt) {
  stopifnot("x" %in% names(dt), 
            "y" %in% names(dt), 
            haskey(dt), 
            key(dt) == "x")
  if (nrow(dt) == 1L) {
    dt[, local_min := TRUE]
    dt[, local_max := TRUE]
  } else {
    dt[, local_min := and(shift(y, fill = first(y), type = "lag") > y, 
                          shift(y, fill = last(y), type = "lead") > y)]
    dt[, local_max := and(shift(y, fill = first(y), type = "lag") < y, 
                          shift(y, fill = last(y), type = "lead") < y)]
    dt[1L, local_min := y[1L] < y[2L]]
    dt[1L, local_max := y[1L] > y[2L]]
    LL <- nrow(dt)
    dt[LL, local_min := y[LL] < y[LL - 1L]]
    dt[LL, local_max := y[LL] > y[LL - 1L]]
  }
  dt
}










