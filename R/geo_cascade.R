#' Geocode a street address
#'
#' First attempts to use the US Census Geocoder (geo_census) method
#' and then Nominatim/OSM (geo_osm) method as a backup
#' Returns latitude and longitude coordinates.
#'
#' @param address Single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @return Latitude and Longitude Coordinates in tibble format
#'
#' @examples
#' geo_cascade("1600 Pennsylvania Ave Washington, DC")
#' geo_cascade("Paris, France")
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @export
geo_cascade = function(address,verbose=FALSE,lat=Lat,long=lng) {

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
