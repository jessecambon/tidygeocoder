#' Geocode a street address using the US Census Geocoder API
#'
#' Returns latitude and longitude coordinates. Only works for US addresses.
#' Address must be at the street level (ie. 60 Main St. Pawnee, IN not Pawnee, IN)
#'
#'
#' @param address Single line address. Street must be included.
#' @param verbose logical. If TRUE outputs logs.
#' @return Latitude and Longitude Coordinates in Tibble format
#'
#' @examples
#' geo_census("1600 Pennsylvania Ave Washington, DC")
#' geo_census("600 Montgomery St, San Francisco, CA 94111")

#' @importFrom httr GET content
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom jsonlite fromJSON
#' @export
geo_census <- function(address,verbose=FALSE){
  benchmark <- 4
  if (verbose == TRUE) {
    print(address)
  }
  # API Call
  base <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?"
  soup <- httr::GET(url=base,query=list(address=address,format='json',benchmark=benchmark))
  dat <- jsonlite::fromJSON(httr::content(soup,as='text',encoding = "ISO-8859-1"), simplifyVector=TRUE)

  coords <- dat$result$addressMatches$coordinates

  # Return lat/lng in tibble form
  if (!is.null(coords)) {
    tibble::tibble(lat=coords$y[1],lng=coords$x[1]) }
  else {
    tibble::tibble(lat=numeric(),lng=numeric()) }
}
