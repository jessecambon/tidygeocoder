#' Geocode a street address
#'
#' First uses the US Census Geocoder (geo_census)
#' and then Nominatim/OSM (geo_osm)
#' Returns latitude and longitude coordinates.
#'
#' @param address Single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @return Latitude and Longitude Coordinates in tibble format
#'
#' @examples
#' geo_cascade("1600 Pennsylvania Ave Washington, DC")
#' geo_cascade("Paris, France")
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @export
geo_cascade = function(address,verbose=FALSE) {
  census <- geo_census(address,verbose=verbose)

  if (nrow(census) > 0) {
    census %>% dplyr::mutate(method='Census')
  } else {
    geo_osm(address,verbose=verbose) %>% dplyr::mutate(method='OSM')
  }
}
