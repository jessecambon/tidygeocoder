#' Geocode addresses
#'
#' Obtains latitude and longitude coordinates from an address using
#' the Nominatim (OSM) geocoder service. Can be used with non-US
#' or non-street level addresses unlike the Census geocoder. This
#' function calls the geocode_OSM function from tmaptools package.
#'
#' WARNING - This service has a usage limit and it will return
#' missing coordinates once the usage limit is reached.
#'
#' @param address single line address
#' @param verbose logical. If TRUE outputs logs.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @return latitude and longitude Coordinates in tibble format
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
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  if (verbose == TRUE) { print(address)}

  # extract coordinates
  coords = unname(tmaptools::geocode_OSM(address)$coords)

  # flip coordinates to output lat,long
  if (!is.null(coords)) { tibble::tibble(!!lat:=coords[2],!!long:=coords[1]) }
  else { tibble::tibble(!!lat:=numeric(),!!long:=numeric()) }
}
