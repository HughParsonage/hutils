#' Maximum area given x and y coordinates
#' @description Present since \code{hutils 1.2.0}.
#' @param DT,x,y Coordinates of a curve containing a rectangle. 
#' Either as a list, \code{DT}, containing columns \code{x} and \code{y}.
#' @param minH The minimum height of the rectangles.
#' @param minW The minimum width of the rectangles.
#' @param maximize How the rectangle should be selected. Currently, only \code{"area"} supported.
#' @param incl_negative Should areas below the x-axis be considered?
#' 
#' @return A \code{data.table}: The coordinates of a rectangle, from (0, 0), (1, 0), (1, 1), (0, 1), south-west clockwise, 
#' that is contained within the area of the chart for positive values only.
#' 
#' @examples ahull(, c(0, 1, 2, 3, 4), c(0, 1, 2, 0, 0))
#' 
#' @export ahull



ahull <- function(DT, x = DT$x, y = DT$y, minH = 0, minW = 0, maximize = "area", incl_negative = FALSE) {
  if (length(y) == 1L || length(y) == 1L) {
    warning("`y` or `x` is length-one. Unlikely ahull() will be useful.")
  }
  dt <- data.table(x, y)
  setkeyv(dt, 'x')
  local_min <- NULL
  set_local_extrema(dt)
  dt[y <= 0, local_min := FALSE]
  
  if (!any(dt[['local_min']])) {
    # All
    n <- nrow(dt)
    area <- double(n)
    h <- double(n)
    w <- double(n)
    xmin <- double(n)
    xmax <- double(n)
    for (i in seq_along(area)) {
      xi <- x[i]
      yi <- y[i]
      if (yi > 0 && i < n) {
        for (j in seq.int(i + 1L, n)) {
          if (y[j - 1L] > yi && y[j] < yi && y[j] < y[j - 1L]) {
            fall <- y[j - 1L] - y[j]
            run  <- x[j] - x[j - 1L]
            slope <- fall / run
            x2 <- x[j - 1] + (y[j - 1L] - yi) / slope
            area[i] <- yi * (x2 - xi)
            h[i] <- yi
            xmin[i] <- xi
            xmax[i] <- x2
            w[i] <- x2 - xi
            break
          }
        }
      }
    }
    area_from_minima <- 
      list(ii = seq_along(area), 
           h = h, 
           w = w,
           xmin = xmin,
           xmax = xmax, 
           area = area)
    setDT(area_from_minima)
  } else {
    area_from_minima <- rbindlist(lapply(which(dt[["local_min"]]), area_from_min, dt = dt))
  }
  setnames(area_from_minima, "ii", "x_stalactite")
  if (incl_negative) {
    negative_area_from_minima <- 
      ahull(x = x,
            y = -y,
            minH = minH,
            minW = minW,
            maximize = maximize,
            incl_negative = FALSE)
    negative <- NULL
    area_from_minima <- 
      rbind(area_from_minima[, negative := FALSE], 
            negative_area_from_minima[, negative := TRUE], 
            use.names = TRUE, 
            fill = TRUE)
    
  } else {
    area_from_minima[, negative := FALSE]
  }
  
  
  area_from_minima <- area_from_minima[h >= minH]
  area_from_minima <- area_from_minima[w >= minW]
  
  ymin <- ymax <- NULL
  area_from_minima[(!negative), ymin := 0]
  area_from_minima[(negative), ymax := 0]
  
  area_from_minima[(!negative), ymax := h]
  area_from_minima[(negative), ymin := -h]
  switch(maximize, 
         "area" = area_from_minima[which.max(area)], 
         area_from_minima[which.max(area)])
  
}

set_local_extrema <- function(dt) {
  y <- NULL
  stopifnot("x" %in% names(dt), 
            "y" %in% names(dt), 
            haskey(dt), 
            key(dt) == "x")
  LL <- nrow(dt)
  local_min <- local_max <- NULL
  if (LL == 1L) {
    dt[, local_min := TRUE]
    dt[, local_max := TRUE]
  } else {
    dt[, local_min := and(shift(y, fill = first(y), type = "lag") > y, 
                          shift(y, fill = last(y), type = "lead") > y)]
    dt[, local_max := and(shift(y, fill = first(y), type = "lag") < y, 
                          shift(y, fill = last(y), type = "lead") < y)]
    
    y <- .subset2(dt, "y")
    
    set(dt, i = 1L, j = "local_min", value = y[1L] < y[2L])
    set(dt, i = 1L, j = "local_max", value = y[1L] > y[2L])
    
    set(dt, i = LL, j = "local_min", value = y[LL] < y[LL - 1L])
    set(dt, i = LL, j = "local_max", value = y[LL] > y[LL - 1L])
  }
  dt
}
  
#           .(x2 y2)
#          / \
#         /   \
# (x1 y1)/     \
#               \.(x3 y3)  
  

A <- function(x1, y1, x2, y2, x3, y3) {
  stopifnot(x1 < x2, x2 < x3, 
            y1 < y2, y2 > y3, y3 <= y1, y3 >= 0)
  w <- x3 - x2 + ((x2 - x3) * (y1 - y3)) / (y2 - y3)
  h <- y2 - y3
  list(w * h / 2, x2 + w)
}

area_from_min <- function(ii, dt) {
  x <- .subset2(dt, "x")
  # Only want the positive
  # Negative values don't reflect the area chart
  y <- pmax.int(.subset2(dt, "y"), 0)
  n <- length(x)
  H <- y[ii]
  if (H == 0) {
    return(list(ii = ii,
                h = H,
                w = NA_real_,
                xmin = NA_real_, 
                xmax = NA_real_,
                area = 0))
  }
  
  
  x_1 <- x[1]
  x1_endpoint <- TRUE
  if (ii > 1L) {
    for (i in seq.int(ii - 1L, 2L)) {
      if (i > 1L && y[i - 1L] < H && y[i] > H) {
        x_02 <- x[i]
        x_01 <- x[i - 1L]
        y_02 <- y[i]
        y_01 <- y[i - 1L]
        x_1 <- x_01 + (H - y_01) * (x_02 - x_01) / (y_02 - y_01)
        x1_endpoint <- FALSE
        break
      }
    }
  }
  
  if (x1_endpoint && y[1L] == 0) {
    return(list(ii = ii,
                h = H,
                w = NA_real_,
                xmin = NA_real_, 
                xmax = NA_real_,
                area = 0))
  }
  
  x_2 <- x[n]
  x2_endpoint <- TRUE
  if (ii < n) {
    for (i in seq.int(ii + 1L, n - 1L)) {
      if (y[i - 1L] > H && y[i] < H) {
        x_02 <- x[i]
        x_01 <- x[i - 1L]
        y_01 <- y[i]
        y_02 <- y[i - 1L]
        
        x_2 <- x_02 - (H - y_01) * (x_02 - x_01) / (y_02 - y_01)
        x2_endpoint <- FALSE
        break
      }
    }
    
    if (x2_endpoint && y[n] == 0) {
      return(list(ii = ii,
                  h = H,
                  w = NA_real_,
                  xmin = NA_real_, 
                  xmax = NA_real_,
                  area = 0))
    }
  }
  
  list(ii = ii,
       h = H,
       w = x_2 - x_1,
       xmin = x_1, 
       xmax = x_2,
       area = y[ii] * {x_2 - x_1})
}

areas_right_of <- function(dt, return_ind = TRUE) {
  x <- .subset2(dt, "x")
  y <- .subset2(dt, "y")
  areas <- X2 <- matrix(NA_real_, 
                        ncol = length(x),
                        nrow = length(x))
  max_area <- 0
  for (i1 in seq_along(x)) {
    for (i3 in seq_along(x)) {
      if (i3 > i1 + 1L) {
        y1 <- y[i1]
        y3 <- y[i3]
        # prev was local min
        if (y[i3 - 2L] > y[i3 - 1L] &&
            y[i3] > y[i3 - 1L]) {
          break
        }
        
        if (y3 <= y1 && y3 >= 0) {
          for (i2 in seq.int(i1 + 1, i3 - 1)) {
            y2 <- y[i2]
            if (y2 > y1 && y2 > y3) {
              x1 <- x[i1]
              x2 <- x[i2]
              x3 <- x[i3]
              Area_X2 <- A(x1, y1, x2, y2, x3, y3)
              if (max_area < Area_X2[[1L]]) {
                max_area <- Area_X2[[1L]]
                out_x <- x1
                out_y <- x3
              }
              areas[i1, i3] <- Area_X2[[1L]]
              X2[i1, i3] <- Area_X2[[2L]]
            }
          }
        }
        
      }
    }
  }
  if (return_ind) {
    return(c(out_x, out_y))
  }
  list(areas, X2)
}

#' @noRd
#' @param y0 A height
#' @param x,y Coordinates of a curve
height2x <- function(h, x, y) {
  if (is.unsorted(x)) {
    stop("`x` must be sorted.")
  }
  y_above <- y >= h
  y_below <- y < h
  
  intersections <- 
    c(which(y_above & shift(y_below, type = "lead")),
      which(y_below & shift(y_above, type = "lead")))
  x_1 <- x[intersections]
  x_2 <- x[intersections + 1L]
  y_1 <- y[intersections]
  y_2 <- y[intersections + 1L]
  x_1 + {x_2 - x_1} * {h - y_1} / {y_2 - y_1}
}








