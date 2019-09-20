#' @importFrom httr GET content
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate
#' @importFrom jsonlite fromJSON

# Return lat/long in tibble form with OSM/Nomanitim Geocoder service
#' @export
osm_latlng <- function(address,verbose=FALSE){
  if (verbose == TRUE) {
    print(address)
  }
  coords = unname(tmaptools::geocode_OSM(address)$coords)

  # flip coordinates to output lat,lng
  if (!is.null(coords)) { tibble::tibble(lat=coords[2],lng=coords[1]) }
  else { tibble::tibble(lat=numeric(),lng=numeric()) }
}

# Return lat/long in tibble form using US Census Geocoder API
# Note that this only works for US addresses and the street address is required
#' @export
census_latlng <- function(address,benchmark=4,verbose=FALSE){
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


# Return Census results if they exist, else return OSM
#' @export
cascade_latlng = function(address,verbose=FALSE) {
  census <- census_latlng(address,verbose=verbose)

  if (nrow(census) > 0) {
    census %>% dplyr::mutate(method='Census')
  } else {
    osm_latlng(address,verbose=verbose) %>% dplyr::mutate(method='OSM')
  }
}
