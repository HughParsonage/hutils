#' Distance between two points on the Earth
#' @param lat1,lon1,lat2,lon2 That latitudes and longitudes of the two points.
#' @return The distance in kilometres between the two points.
#' @details This is reasonably accurate for distances in the order of 1 to 1000 km.
#' 
#' @examples 
#' 
#' # Distance from YMEL to YSSY
#' haversine_distance(-37 - 40/60, 144 + 50/60, -33 - 56/60, 151 + 10/60)
#' 
#' @export

haversine_distance <- function(lat1, lon1, lat2, lon2){
  # to radians
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  
  delta_lat <- abs(lat1 - lat2)
  delta_lon <- abs(lon1 - lon2)
  
  # radius of earth
  6371 * 2 * asin(
    sqrt(
      `+`((sin(delta_lat / 2)) ^ 2, 
          cos(lat1) * cos(lat2) * (sin(delta_lon / 2)) ^ 2
      )
    )
  )
}


