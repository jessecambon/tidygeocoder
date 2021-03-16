selected_method <- "bing"

lat <- 38.89586
lon <- -77.0307713


api_url <- tidygeocoder:::get_bing_url()
api_url <- paste0(api_url,'/', lat,',',lon)

api_url

api_query_parameters <- list(maxResults = 1,
                             key = tidygeocoder:::get_key(selected_method) )

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
api_url
query_results <- query_api(api_url, api_query_parameters)
raw_results <- jsonlite::fromJSON(query_results$content)

raw_results$resourceSets$resources[[1]]['name']

results_minimal <-
  tidygeocoder::extract_reverse_results(selected_method, raw_results, full_results = FALSE)

results_minimal

full_results_notflat <-
  tidygeocoder::extract_reverse_results(selected_method,
                                raw_results,
                                full_results = TRUE,
                                flatten = FALSE
  )
full_results_notflat


full_results_flat <-
  tidygeocoder::extract_reverse_results(selected_method,
                                raw_results,
                                full_results = TRUE,
                                flatten = TRUE
  )
full_results_flat

# Test reverse_geo ----
library(tibble)

lat <- 40.4055517
lon <- -3.6802152

# Errors
tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "bing",
  custom_query = list(
    key = "xxx"
  )
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "bing",
  custom_query = list(
    verboseplacenames  = "xxx"
  )
)
# End errors

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "bing",
  full_results = FALSE,
  limit = 7
)



tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "bing",
  full_results = FALSE,
  limit = 1
)


livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "bing"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "bing"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "bing"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "bing",
    mode = "single",
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
    long = longitud, lat = latitud, method = "bing", address = "dir",
    full_results = TRUE,
    verbose = TRUE,
    mode = "single",
    custom_query = list(
      language = "pl"
    ),
  )
glimpse(address)
