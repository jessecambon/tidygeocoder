#' Geocode addresses with Nomaninatim (OSM)
#'
#' Obtains latitude and longitude coordinates from an address using
#' the Nominatim (OSM) Geocoder API. Can be used with non-US
#' or non-street level addresses unlike the Census geocoder
#'
#' @param address Single line address
#' @param verbose logical. If TRUE outputs logs.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @return Latitude and Longitude Coordinates in Tibble format
#'
#' @examples
#' geo_osm("1600 Pennsylvania Ave Washington, DC")
#' geo_osm("Paris, France")

#' @importFrom tmaptools geocode_OSM
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom rlang ':=' enquo
#' @export
geo_osm <- function(address,verbose=FALSE,lat=lat,long=long){
  latitude  <- longitude <- NULL # prevents 'no visible binding for global variable' warning
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  if (verbose == TRUE) { print(address)}

  coords = unname(tmaptools::geocode_OSM(address)$coords)

  # flip coordinates to output lat,lng
  if (!is.null(coords)) { tibble::tibble(!!lat:=coords[2],!!long:=coords[1]) }
  else { tibble::tibble(!!lat:=numeric(),!!long:=numeric()) }
}
