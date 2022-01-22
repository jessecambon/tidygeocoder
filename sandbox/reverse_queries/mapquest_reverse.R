selected_method <- "mapquest"

lat <- 40.4606543
lon <- -4.205381


api_url <- tidygeocoder:::get_mapquest_url(reverse = TRUE)
api_query_parameters <- list(maxResults = 1,
                             location = paste0(as.character(lat), ",", as.character(lon)),
                             key = tidygeocoder:::get_key(selected_method) )

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----

query_results <- query_api(api_url, api_query_parameters)
raw_results <- jsonlite::fromJSON(query_results$content)
cols <- c('street', paste0('adminArea', seq(1, 6)))

tidygeocoder:::format_address(raw_results$results$locations[[1]],
                             method ="mapquest")


df <- tibble::as_tibble(raw_results$results$locations[[1]])


df <- dplyr::relocate(df,intersect(cols, names(df)))
df
results_minimal <-
  tidygeocoder::extract_reverse_results(selected_method, raw_results, full_results = FALSE)

results_minimal

full_results_notflat <-
  tidygeocoder::extract_results(selected_method,
                                raw_results,
                                full_results = TRUE,
                                flatten = FALSE
  )
full_results_notflat

full_results_notflat

full_results_flat <-
  tidygeocoder::extract_results(selected_method,
                                raw_results,
                                full_results = TRUE,
                                flatten = TRUE
  )
full_results_flat

# Errors

error <- query_api(api_url, list(location = addr,
                                 key = "aa"))

error
extract_errors_from_results('mapquest',error$content,TRUE)

error2 <- query_api(api_url, list(location = addr,
                                  key = tidygeocoder:::get_key(selected_method),
                                  boundingBox=4))
raw_results <- jsonlite::fromJSON(error2$content)

raw_results$info$messages
names(raw_results)
jsonlite::fromJSON(error2$content)
httr::warn_for_status(400)
jsonlite::fromJSON(error2$content)$info$statuscode
extract_errors_from_results('mapquest',error$content,TRUE)


# Test reverse_geo ----
library(tibble)

lat <- 40.4055517
lon <- -3.6802152

# Errors
tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "mapquest",
  custom_query = list(
    key = "xxx"
  )
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "mapquest",
  custom_query = list(
    thumbMaps  = "xxx"
  )
)
tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "mapquest",
  mapquest_open = "TRUE"
)
# End errors

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "mapquest",
  full_results = FALSE,
  limit = 7
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  method = "mapquest",
  full_results = FALSE,
  limit = 7,
  mapquest_open = TRUE
)


livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "mapquest"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "mapquest"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "mapquest"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "mapquest",
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
    long = longitud, lat = latitud, method = "mapquest", address = "dir",
    full_results = TRUE,
    verbose = TRUE,
    mode = "single",
    custom_query = list(
      language = "pl"
    ),
  )
glimpse(address)
