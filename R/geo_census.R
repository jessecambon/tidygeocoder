#' Geocode a street address using the US Census Geocoder API
#'
#' Returns latitude and longitude coordinates. Only works for US addresses.
#' Address must be at the street level (ie. 60 Main St. Pawnee, IN not Pawnee, IN)
#'
#'
#' @param address Single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param API_URL URL of Census API
#' @importFrom httr GET content
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom jsonlite fromJSON
#' @importFrom rlang enquo ":="
#' @export
geo_census <- function(address,verbose=FALSE,lat=latitude,long=longitude,
                       API_URL="https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?"){
  latitude  <- longitude <- NULL # prevents 'no visible binding for global variable' warning
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)


  benchmark <- 4
  if (verbose == TRUE) {
    print(address)
  }
  # API Call
  soup <- httr::GET(url=API_URL,query=list(address=address,format='json',benchmark=benchmark))
  dat <- jsonlite::fromJSON(httr::content(soup,as='text',encoding = "ISO-8859-1"), simplifyVector=TRUE)

  coords <- dat$result$addressMatches$coordinates

  # Return lat/lng in tibble form
  if (!is.null(coords)) {
    tibble::tibble(!!lat:=coords$y[1],!!long:=coords$x[1]) }
  else {
    tibble::tibble(!!lat:=numeric(),!!long:=numeric()) }
}
