#' @title Bearing calculations
#' @param lat_orig,lon_orig,lat_dest,lon_dest Latitude and longitude
#' of origin and destination.
#' @param compass A character vector of compass rose points, such as 
#' \code{c("NW", "E", "SSW")}.
#' 
#' @return
#' \describe{
#' \item{\code{bearing}}{An approximate bearing from \code{_orig} and \code{_dest}.}
#' \item{\code{compass2bearing}}{The bearing encoded by the compass input.}
#' \item{\code{easterly_component}}{The easterly component of a unit vector
#' pointing in the direction provided.}
#' }
#' 
#' @examples
#' bearing(0, 0, 90, 0)
#' bearing(-35, 151, 51, 0)
#' 
#' compass2bearing("NW")
#' easterly_component("E")
#' easterly_component("NW")
#' 
#' 
#' @export

bearing <- function(lat_orig, lon_orig, lat_dest, lon_dest) {
  toRad <- pi/180
  prohibit_vector_recycling(lat_orig, lon_orig, lat_dest, lon_dest)
  
  lat1 <- lat_orig * toRad
  lat2 <- lat_dest * toRad
  lon1 <- lon_orig * toRad
  lon2 <- lon_dest * toRad

  dLon <- (lon2 - lon1)
  dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
  i <- (abs(dLon) > pi)
  j <- i & dLon > 0
  dLon[j] <- -(2 * pi - dLon[j])
  j <- i & dLon <= 0
  dLon[j] <- (2 * pi + dLon[j])
  b <- atan2(dLon, dPhi)
  b <- b/toRad
  (b + 360) %% 360
}

#' @rdname bearing
#' @export
compass2bearing <- function(compass) {
  CompassNames <- c("CALM", 
                    "E", "ENE", "ESE",
                    "N",
                    "NE", "NNE", "NNW", "NW", 
                    "S", 
                    "SE", "SSE", "SSW", "SW",
                    "W", "WNW", "WSW")
  
  m <- match(toupper(compass), CompassNames)
  
  b <- c(0, 
         90, 67.5, 112.5, 
         0, 
         45, 22.5, 337.5, 315,
         180, 
         135, 157.5, 202.5, 225,
         270, 292.5, 247.5)
  b[m]
}

#' @rdname bearing
#' @export
easterly_component <- function(compass) {
  BearingDeg <- compass2bearing(compass)
  sinpi(BearingDeg / 180)
}

#' @rdname bearing
#' @export
northerly_component <- function(compass) {
  BearingDeg <- compass2bearing(compass)
  cospi(BearingDeg / 180)
}


