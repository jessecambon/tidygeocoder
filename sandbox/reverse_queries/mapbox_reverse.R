selected_method <- "mapbox"

# https://docs.mapbox.com/api/search/geocoding/#reverse-geocoding

url_base <- tidygeocoder:::get_mapbox_url()

lat <- 40.4055517
lon <- -3.6802152

soup <-
  httr::GET(
    url = gsub(" ", "%20", paste0(url_base, lon, ",", lat, ".json")),
    query = list(
      limit = 1,
      access_token = tidygeocoder:::get_key(selected_method)
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

results_min <-
  tidygeocoder:::extract_reverse_results(selected_method, response,
    full_results = FALSE
  )

results_full <-
  tidygeocoder:::extract_reverse_results(selected_method, response)

full_results_notflat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = FALSE
  )
full_results_flat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = TRUE
  )

## Test reverse_geo

livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "mapbox"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "mapbox"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "mapbox"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.858296, 40.4530541),
    long = c(2.294479, -3.6883445),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "mapbox",
    custom_query = list(
      language = "pl",
      types = "address"
    ),
  )

glimpse(livetest_params)


library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with long lat
some_lonlat <- tribble(
  ~latitud, ~longitud,
  40.4165021, -3.7025642,
  41.3887869, 2.1589851,
  39.4697524, -0.3773868
)

# geocode the addresses

address <- some_lonlat %>%
  reverse_geocode(
    long = longitud, lat = latitud, method = "mapbox", address = "dir",
    full_results = TRUE,
    custom_query = list(
      language = "pl",
      types = "postcode"
    ),
  )
glimpse(address)
