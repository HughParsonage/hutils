

bearing <- function(lat_orig, lon_orig, lat_dest, lon_dest) {
  toRad <- pi/180
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
