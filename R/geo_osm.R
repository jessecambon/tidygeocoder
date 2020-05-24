#' Geocode addresses
#'
#' Obtains latitude and longitude coordinates from an address using
#' the Nominatim (OSM) geocoder service. Can be used with non-US
#' or non-street level addresses unlike the Census geocoder.
#'
#' WARNING - This service has a usage limit and it will return
#' missing coordinates once the usage limit is reached. The min_time
#' argument defaults to 1 second to avoid this usage limit being reached.
#' 
#' Query adapted from tmaptools::geocode_OSM. 
#' @references tmaptools: \url{https://github.com/mtennekes/tmaptools}
#' @references Nominatim usage policy: \url{https://operations.osmfoundation.org/policies/nominatim}
#' @references Nominatim API reference: \url{https://nominatim.org/release-docs/develop/api/Search/}
#'
#' @param address single line address
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param min_time Minimum time (in seconds) for the query to take. 
#' Defaults to 1 second per the Nominatim usage policy. 
#' Be cautious of setting this value lower as the usage limit may be reached.
#' @param verbose logical. If TRUE outputs logs.
#' @param debug logical. If TRUE outputs debugging logs
#' @param api_url URL to OSM Nominatim server
#' @return latitude and longitude coordinates in tibble format
#'
#' @examples
#' \donttest{
#' geo_osm("1600 Pennsylvania Ave Washington, DC")
#' geo_osm('20 downing st london',debug=TRUE)
#' geo_osm("Paris, France",verbose=TRUE)
#' }
#' @importFrom tibble tibble
#' @export
geo_osm <- function(address,lat=lat, long=long, min_time=1, verbose=FALSE, debug=FALSE,
                    api_url="http://nominatim.openstreetmap.org/search"){
  start_time <- Sys.time() # start timer
  
  lat <- deparse(substitute(lat))
  long <- deparse(substitute(long))
  
  # what to return when we don't find results
  NA_value <- get_na_value(lat,long)
  
  if (verbose == TRUE) { message(address)}

  # what to return if address is invalid or no coordinates are found

  # if address is NA or blank then return NA, else make call to Nominatim geocoder
  # numeric data is allowed but must be in string format (ie. for zip codes)
  if (is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
      return(NA_value)
  } else {
    
    ##########
    # OSM Query
    ##########  
    # addressdetails = 0/1
    # limit = 1 means we only return one query result
    
    res <- query_api(api_url, list(q = address, format = 'json', limit = 1))
    
    if (debug == TRUE) { print(res) }
    
    # If no results found, return NA
    if (length(res) == 0) {
      if (verbose == TRUE) {  message(paste("No results found for \"", address, "\".", sep = "")) }
      return(NA_value)
    }
    
    # Extract latitude and longitude from the search results
    coords <- as.numeric( c(res$lat, res$lon) )
    ##########
    
    ## Make sure the proper amount of time has elapsed for the query per min_time
    pause_until(start_time, min_time, debug = verbose)
    
    # Return  results
    if (!is.null(coords)) {
      
      lat_lng <- tibble::tibble(a = coords[1], b = coords[2])
      colnames(lat_lng) <- c(lat, long)
      return(lat_lng) 
    }
    else return(NA_value)
  }
}
