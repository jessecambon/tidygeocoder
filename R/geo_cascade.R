#' Geocode a street address
#'
#' First attempts to use the US Census Geocoder (geo_census) method
#' and then uses the Nominatim/OSM (geo_osm) method if the census method failed
#' Returns latitude and longitude coordinates.
#'
#' @param address single line address.
#' @param verbose logical. If TRUE outputs logs.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @return latitude and longitude coordinates in tibble format
#'
#' @examples
#' geo_cascade("1600 Pennsylvania Ave Washington, DC")
#' geo_cascade("Paris, France")
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @export
geo_cascade = function(address,verbose=FALSE,lat=lat,long=long) {
  latitude  <- longitude <- NULL # prevents 'no visible binding for global variable' warning

  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  if (verbose == TRUE) {
    print(address)
  }

  census <- geo_census(address,verbose=verbose,lat=!!lat,long=!!long)

  if (nrow(census) > 0) {
    census %>% dplyr::mutate(geo_method='census')
  } else {
    geo_osm(address,verbose=verbose,lat=!!lat,long=!!long) %>% dplyr::mutate(geo_method='osm')
  }
}
