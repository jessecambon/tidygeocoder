#' Geocode a street address using the Nominatim (OSM) Geocoder API
#'
#' Returns latitude and longitude coordinates.
#'
#' @param address Single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @return Latitude and Longitude Coordinates in Tibble format
#'
#' @examples
#' geo_osm("1600 Pennsylvania Ave Washington, DC")
#' geo_osm("Paris, France")

#' @importFrom tmaptools geocode_OSM
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @export
geo_osm <- function(address,verbose=FALSE){
  if (verbose == TRUE) {
    print(address)
  }
  coords = unname(tmaptools::geocode_OSM(address)$coords)

  # flip coordinates to output lat,lng
  if (!is.null(coords)) { tibble::tibble(lat=coords[2],lng=coords[1]) }
  else { tibble::tibble(lat=numeric(),lng=numeric()) }
}
