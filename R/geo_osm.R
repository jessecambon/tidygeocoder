#' Geocode addresses
#'
#' Obtains latitude and longitude coordinates from an address using
#' the Nominatim (OSM) geocoder service. Can be used with non-US
#' or non-street level addresses unlike the Census geocoder.
#'
#' WARNING - This service has a usage limit and it will return
#' missing coordinates once the usage limit is reached.
#' 
#' Adapted from tmaptools::geocode_OSM. 
#' @references \url{https://github.com/mtennekes/tmaptools}
#'
#' @param address single line address
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param verbose logical. If TRUE outputs logs.
#' @param server - URL to OSM Nominatim server
#' @return latitude and longitude coordinates in tibble format
#'
#' @examples
#' \donttest{
#' geo_osm("1600 Pennsylvania Ave Washington, DC")
#' geo_osm("Paris, France",verbose=TRUE)
#' }
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom rlang ':=' enquo
#' @importFrom stringr str_trim
#' @importFrom XML xmlTreeParse xmlChildren xmlRoot xmlAttrs
#' @importFrom utils download.file
#' @export
geo_osm <- function(address,lat=lat, long=long, verbose=FALSE, server="http://nominatim.openstreetmap.org"){
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  if (verbose == TRUE) { message(address)}

  # what to return if address is invalid or no coordinates are found
  NA_value <- tibble::tibble(!!lat := numeric(), !!long := numeric())

  # if address is NA or blank then return NA, else make call to Nominatim geocoder
  # numeric data is allowed but must be in string format (ie. for zip codes)
  if (is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
      return(NA_value)
  } else {
    
    ##########
    # Construct OSM Query
    ##########  
    query <- gsub(" ", "+", enc2utf8(address), fixed = TRUE)
    
    url <- paste0(server, "/search?q=", query, 
                  "&format=xml&polygon=0&addressdetails=0")
    
    # Download search results with a temp file
    tmpfile <- tempfile()
    suppressWarnings(utils::download.file(url, destfile = tmpfile, 
                                   mode = "wb", quiet = TRUE))
    
    # Parse search results.
    doc <- XML::xmlTreeParse(tmpfile, encoding = "UTF-8")
    unlink(tmpfile)
    res <- XML::xmlChildren(XML::xmlRoot(doc))
    
    # If no results found, return NA
    if (length(res) == 0) {
      if (verbose == TRUE) {  message(paste("No results found for \"", address, "\".", sep = "")) }
      return(NA_value)
    }
    
    # Obtain search results
    sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", 
                  "display_name", "class", "type", "importance", "icon")
    search_result <- XML::xmlAttrs(res[[1]])
    search_result_id <- search_result[sn_names]
    names(search_result_id) <- sn_names # in case of missings
    Encoding(search_result_id) <- "UTF-8"
    
    # Extract latitude and longitude from the search results
    lat_lon <- search_result[c("lat","lon")]
    
    coords <- as.numeric(lat_lon) 
    ##########

    # Return  results
    if (!is.null(coords)) { tibble::tibble(!!lat := coords[1], !!long := coords[2]) }
    else {return(NA_value)}
  }
}
