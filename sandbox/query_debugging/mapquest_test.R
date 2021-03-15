selected_method <- "mapquest"

addr <- "Acueducto de Segovia, Spain"


api_url <- tidygeocoder:::get_mapquest_url()
api_query_parameters <- list(maxResults = 1,
                             location = addr,
                             key = tidygeocoder:::get_key(selected_method) )

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----

query_results <- query_api(api_url, api_query_parameters)
query_results

raw_results <- jsonlite::fromJSON(query_results$content)

results_minimal <-
  tidygeocoder::extract_results(selected_method, raw_results, full_results = FALSE)

results_minimal

tidygeocoder::extract_results(selected_method, raw_results)

full_results_notflat <-
  tidygeocoder::extract_results(selected_method,
    raw_results,
    full_results = TRUE,
    flatten = FALSE
  )
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

# Test geo ----
library(tibble)
addr <- "Plaza Mayor"

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "mapquest",
  limit = 5,
)
# Errors
tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "mapquest",
  limit = 5,
  custom_query =  list(key="aa")
)

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "mapquest",
  limit = 5,
  custom_query =  list(ignoreLatLngInput ="aa")
)

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  method = "mapquest",
  mapquest_open = "TRUE"
)
# End errors

livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "mapquest"
  )

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  method = "mapquest",
  mapquest_open = TRUE
)

glimpse(livetest)
livetest_full <-
  tidygeocoder::geo(
    address = "Antonio de Leyva, Madrid",
    verbose = TRUE,
    full_results = TRUE,
    method = "mapquest"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "mapquest"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::geo(
    address = c("Santiago de Compostela; Spain", "Nieva"),
    verbose = TRUE,
    full_results = TRUE,
    mode = "single",
    limit = 2,
    custom_query = list(
      language = "fr-FR"
    ),
    method = "mapquest"
  )


library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
  ~name, ~addr,
  "White House", "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",
  "Willis Tower", "233 S Wacker Dr, Chicago, IL 60606"
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr,
    method = "mapquest", lat = latitude, long = longitude,
    full_results = TRUE, mode = "single", verbose = TRUE
  )

lat_longs
