# Required for US Census API call
library(httr)
library(jsonlite)
# Required for OSM
library(tmaptools)
# Required for all
library(tidyverse)


# Return lat/long in tibble form with OSM/Nomanitim Geocoder service
osm_latlng <- function(address,verbose=FALSE){
  if (verbose == TRUE) {
    print(address)
  }
  coords = unname(geocode_OSM(address)$coords)

  # flip coordinates to output lat,lng
  if (!is.null(coords)) { tibble(lat=coords[2],lng=coords[1]) }
  else { tibble(lat=numeric(),lng=numeric()) }
}

# Return lat/long in tibble form using US Census Geocoder API
# Note that this only works for US addresses and the street address is required
census_latlng <- function(address,benchmark=4,verbose=FALSE){
  if (verbose == TRUE) {
    print(address)
  }
  # API Call
  base <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?"
  soup <- GET(url=base,query=list(address=address,format='json',benchmark=benchmark))
  dat <- fromJSON(content(soup,as='text',encoding = "ISO-8859-1"), simplifyVector=TRUE)

  coords <- dat$result$addressMatches$coordinates

  # Return lat/lng in tibble form
  if (!is.null(coords)) {
    tibble(lat=coords$y[1],lng=coords$x[1]) }
  else {
    tibble(lat=numeric(),lng=numeric()) }
}


# Return Census results if they exist, else return OSM
cascade_latlng = function(address,verbose=FALSE) {
  census <- census_latlng(address,verbose=verbose)

  if (nrow(census) > 0) {
    census %>% mutate(method='Census')
  } else {
    osm_latlng(address,verbose=verbose) %>% mutate(method='OSM')
  }
}
