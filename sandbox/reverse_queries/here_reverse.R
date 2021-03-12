selected_method <- "here"

url_base <- tidygeocoder:::get_here_url(reverse = TRUE)

lat <- 40.4055517
lon <- -3.6802152

# Test Sandbox on dev ----

soup <-
  httr::GET(
    url = url_base,
    query = list(
      limit = 1,
      apiKey = tidygeocoder:::get_key(selected_method),
      at = paste0(lat, ",", lon)
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))


tidygeocoder:::extract_reverse_results(selected_method, response,
  full_results = FALSE
)

results_full <-
  tidygeocoder:::extract_reverse_results(selected_method, response)

results_full

full_results_notflat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = FALSE
  )
full_results_notflat

full_results_flat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = TRUE
  )

full_results_flat

# Test reverse_geo ----

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "here"
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "here",
  custom_query = list(
    country = "papa"
  )
)

livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "here"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "here"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "here"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.858296, 40.4530541),
    long = c(2.294479, -3.6883445),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "here",
    custom_query = list(
      lang = "pl"
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
    long = longitud, lat = latitud, method = "here", address = "dir",
    full_results = TRUE,
    custom_query = list(
      lang = "pl"
    ),
  )
glimpse(address)
