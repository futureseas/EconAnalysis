#' Calculate distance based on spherical law of cosines
#' 
#' @param long1 starting longitude point
#' @param lat1 starting latitude point
#' @param long2 ending longitude point
#' @param lat2 ending latitude point
#' @return Distance in km

#' @export

gcd_slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}
