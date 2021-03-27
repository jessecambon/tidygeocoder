selected_method <- "arcgis"


url_base <- tidygeocoder:::get_arcgis_url(reverse = TRUE)

lat <- 40.4055517
lon <- -3.6802152

soup <-
  httr::GET(
    url =url_base,
    query = list(
      f = "json",
      maxLocations = 5,
      location = paste0(lon,",",lat)
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))
names(response$address) != "LongLabel"
response$address['LongLabel']
response$address[names(response$address) != 'LongLabel']
results_min <-
  tidygeocoder:::extract_reverse_results(selected_method, response,
    full_results = FALSE
  )

results_min

results_min

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

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "arcgis",
  custom_query = list(
    key = "xxx"
  )
)

fullwithparams <- tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  full_results = TRUE,
  method = "arcgis",
  custom_query = list(
    langCode="DE",
    outFields="*"
  )
)

glimpse(fullwithparams)
tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  address = "dirdir",
  method = "arcgis",
  full_results = TRUE,
  limit = 7
)



livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "arcgis"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "arcgis"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "arcgis"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "arcgis",
    mode = "single",
    custom_query = list(
      featureTypes = "POI"
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
    long = longitud, lat = latitud, method = "arcgis", address = "dir",
    full_results = TRUE,
    verbose = TRUE,
    mode = "single",
    custom_query = list(
      language = "pl"
    ),
  )
glimpse(address)

# Error on format

  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "arcgis",
    mode = "single",
    custom_query = list(
      f = "POI"
    ),
  )
  
  # Error on API QUery
  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = TRUE,
    limit = 3,
    method = "arcgis",
    mode = "single",
    custom_query = list(
      token = "POI"
    ),
  )
  