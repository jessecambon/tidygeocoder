#' Geocode addresses
#'
#' First attempts to use the US Census Geocoder (geo_census) method
#' and then uses the Nominatim/OSM (geo_osm) method if the census method failed.
#' Returns latitude and longitude coordinates and the method used to return results (OSM or Census)
#'
#' @param address single line address.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param verbose logical. If TRUE outputs logs.
#' @return latitude and longitude coordinates and the geocoder method used (geo_method) in tibble format (3 columns)
#'
#' @examples
#' \donttest{
#' geo_cascade("1600 Pennsylvania Ave Washington, DC")
#' geo_cascade("Paris, France")
#' }
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom stringr str_trim
#' @export
geo_cascade = function(address,lat=lat,long=long,verbose=FALSE) {
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  NA_value <- tibble::tibble(!!lat:=numeric(),!!long:=numeric()) %>% dplyr::mutate(geo_method="")

  if (verbose == TRUE) {message(address)}

  # if address is NA or blank then return NA, else attempt to geocode
  # (OSM can geocode zip codes so we allow numeric addresses)
  if (is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
    NA_value
  } else {

    # First attempt to use OSM
    census <- geo_census(address,verbose=verbose,lat=!!lat,long=!!long)

    # If the census method fails, then we will try OSM
    if (nrow(census) > 0) {
      census %>% dplyr::mutate(geo_method='census')
    } else {
      osm <- geo_osm(address,verbose=verbose,lat=!!lat,long=!!long)

      # If osm method fails, just return NA
      if (nrow(osm) > 0 ) {
        osm %>% dplyr::mutate(geo_method='osm')
      }
      else {
        NA_value
      }
    }
  }
}
