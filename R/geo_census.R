#' Geocode street addresses
#'
#' Obtain latitude and longitude coordinates from an address using the US Census geocoder.
#' Only works for addresses within the US. Addresses must also be at the street
#' level (ie. 60 Main St. Pawnee, IN not Pawnee, IN).
#' 
#' @references /href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census API}
#'
#' @param address single line address. Street must be included.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param verbose logical. If TRUE outputs logs.
#' @param API_URL URL of Census API
#' @param benchmark parameter for the US Census Geocoder
#' @return latitude and longitude coordinates in tibble format
#'
#' @examples
#' \donttest{
#' geo_census("1600 Pennsylvania Ave Washington, DC")
#' }
#' @importFrom httr GET content
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom jsonlite fromJSON
#' @importFrom rlang enquo ":="
#' @importFrom stringr str_trim
#' @export
geo_census <- function(address, lat = lat, long = long, verbose=FALSE,
       benchmark=4,
       api_url="https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"){
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  # what to return if address is invalid or no coordinates are found
  NA_value <- tibble::tibble(!!lat := numeric(), !!long := numeric())

  if (verbose == TRUE) { message(address) }

  # if address is NA, numeric, or blank then return NA, else make call to census geocoder
  if (!is.na(suppressWarnings(as.numeric(address))) | !is.character(address) | is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
    return(NA_value)
  }
    # API Call
    dat <- query_api(api_url, 
              list(address = address, format = 'json', benchmark = benchmark), 
              content_encoding = "ISO-8859-1")
    
    #soup <- httr::GET(url = api_url, query = list(address = address, format = 'json', benchmark = benchmark))
    #dat <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "ISO-8859-1"), simplifyVector = TRUE)

    # extract coordinates
    coords_xy <- dat$result$addressMatches$coordinates

    # Return coordinates in tibble form
    if (!is.null(coords)) return(tibble::tibble(!!lat := coords_xy$y[1], !!long := coords_xy$x[1]))
    else return(NA_value) 
}
