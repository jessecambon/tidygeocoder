#' Geocode addresses
#'
#' Obtains latitude and longitude coordinates from an address using
#' the Nominatim (OSM) geocoder service. Can be used with non-US
#' or non-street level addresses unlike the Census geocoder. This
#' function calls the geocode_OSM function from the tmaptools package.
#'
#' WARNING - This service has a usage limit and it will return
#' missing coordinates once the usage limit is reached.
#'
#' @param address single line address
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param verbose logical. If TRUE outputs logs.
#' @return latitude and longitude coordinates in tibble format
#'
#' @examples
#' \donttest{
#' geo_osm("1600 Pennsylvania Ave Washington, DC")
#' geo_osm("Paris, France",verbose=TRUE)
#' }
#' @importFrom tmaptools geocode_OSM
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom rlang ':=' enquo
#' @importFrom stringr str_trim
#' @export
geo_osm <- function(address,lat=lat,long=long,verbose=FALSE){
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  if (verbose == TRUE) { message(address)}

  # what to return if address is invalid or no coordinates are found
  NA_value <- tibble::tibble(!!lat:=numeric(),!!long:=numeric())

  # if address is NA or blank then return NA, else make call to Nominatim geocoder
  # numeric data is allowed but must be in string format (ie. for zip codes)
  if (is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
    NA_value
  } else {
    # extract coordinates
    coords = unname(tmaptools::geocode_OSM(address)$coords)

    # flip coordinates to output lat,long
    if (!is.null(coords)) { tibble::tibble(!!lat:=coords[2],!!long:=coords[1]) }
    else { NA_value }
  }
}
