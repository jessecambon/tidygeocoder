selected_method <- "geocode.xyz"


url_base <- tidygeocoder:::get_geocodexyz_url()
# 51.50354,-0.12768


lat <- 40.928958896602715
lon <- -4.310913024933394

# lat <- 51.50354
# lon <- -0.12768
# 
# lat <- 40.4055517
# lon <- -3.6802152

soup <-
  httr::GET(
    url = url_base,
    query = list(
      locate = paste0(lat, ",", lon),
      geoit = "json"
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))


format_address(df=response, "geocode.xyz")

results_min <-
  tidygeocoder:::extract_reverse_results(selected_method, response,
    full_results = FALSE
  )

results_min
method = selected_method
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
library(tibble)

lat <- 40.4055517
lon <- -3.6802152

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "geocode.xyz",
  verbose = TRUE
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  method = "geocode.xyz",
  
)

tidygeocoder::reverse_geo(
  lat = lat,
  long = lon,
  verbose = TRUE,
  address = "dirdir",
  method = "geocode.xyz",
  full_results = TRUE,
  limit = 7
)



livetest <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    method = "geocode.xyz"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    method = "geocode.xyz"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::reverse_geo(
    lat = lat,
    long = lon,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "geocode.xyz"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::reverse_geo(
    lat = c(48.85, 40.45),
    long = c(2.29, -3.68),
    verbose = TRUE,
    full_results = FALSE,
    limit = 3,
    method = "geocode.xyz"
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
    long = longitud, lat = latitud, method = "geocode.xyz", address = "dir",
    full_results = FALSE,
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
    method = "geocode.xyz",
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
    method = "geocode.xyz",
    mode = "single",
    custom_query = list(
      token = "POI"
    ),
  )
  