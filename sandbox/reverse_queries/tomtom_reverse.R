selected_method <- "tomtom"


url_base <- tidygeocoder:::get_tomtom_url(reverse = TRUE)

lat <- 40.4055517
lon <- -3.6802152

soup <-
  httr::GET(
    url = gsub(" ", "%20", paste0(url_base, lat, ",", lon, ".json")),
    query = list(
      limit = 1,
      key = tidygeocoder:::get_key(selected_method)
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))
response$addresses$address['freeformAddress']
results_min <-
  tidygeocoder:::extract_reverse_results(selected_method, response,
    full_results = FALSE
  )

response
results_full <-
  tidygeocoder:::extract_reverse_results(selected_method, response)
results_full

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

# Test reverse_geo ----
library(tibble)

lat <- 40.4055517
lon <- -3.6802152
# Error
tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "tomtom",
  custom_query = list(
    country="papa"
  )
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "tomtom",
  address = "direct",
  limit = 7
)



livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "tomtom"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "tomtom"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "tomtom"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "tomtom",
    custom_query = list(
      language = "pl"
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
    long = longitud, lat = latitud, method = "tomtom", address = "dir",
    full_results = TRUE,
    custom_query = list(
      language = "pl"
    ),
  )
glimpse(address)
