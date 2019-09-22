#' Geocode a street address using the US Census Geocoder API
#'
#' Returns latitude and longitude coordinates. Only works for US addresses.
#' Address must be at the street level (ie. 60 Main St. Pawnee, IN not Pawnee, IN)
#'
#'
#' @param address Single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @param latitude name of latitude field
#' @param longitude name of longitude field
#' @param API_URL URL of Census API
#' @importFrom httr GET content
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom jsonlite fromJSON
#' @export
geo_census <- function(address,verbose=FALSE,latitude="lat",longitude="lng",
                       API_URL="https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?"){
  latitude <- rlang::enquo(latitude)
  longitude <- rlang::enquo(longitude)


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
    tibble::tibble(!!latitude:=coords$y[1],!!longitude:=coords$x[1]) }
  else {
    tibble::tibble(!!latitude:=numeric(),!!longitude:=numeric()) }
}
