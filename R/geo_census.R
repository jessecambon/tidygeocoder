#' Geocode street addresses
#'
#' Obtain latitude and longitude coordinates from an address using the US Census geocoder.
#' Only works for addresses within the US. Addresses must also be at the street
#' level (ie. 60 Main St. Pawnee, IN not Pawnee, IN).
#'
#' @param address single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param API_URL URL of Census API
#' @param benchmark parameter for the US Census Geocoder
#' @importFrom httr GET content
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom jsonlite fromJSON
#' @importFrom rlang enquo ":="
#' @export
geo_census <- function(address,verbose=FALSE,lat=lat,long=long,
                       benchmark=4,
                       API_URL="https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?"){
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  if (verbose == TRUE) { print(address) }

  # API Call
  soup <- httr::GET(url=API_URL,query=list(address=address,format='json',benchmark=benchmark))
  dat <- jsonlite::fromJSON(httr::content(soup,as='text',encoding = "ISO-8859-1"), simplifyVector=TRUE)

  # extract coordinates
  coords <- dat$result$addressMatches$coordinates

  # Return coordinates in tibble form
  if (!is.null(coords)) {
    tibble::tibble(!!lat:=coords$y[1],!!long:=coords$x[1]) }
  else {
    tibble::tibble(!!lat:=numeric(),!!long:=numeric()) }
}
