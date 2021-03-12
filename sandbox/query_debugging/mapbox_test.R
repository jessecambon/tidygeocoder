selected_method <- "mapbox"

addr <- "Acueducto de Segovia, Spain"
url_base <- tidygeocoder:::get_mapbox_url()

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url = gsub(" ", "%20", paste0(url_base, addr, ".json")),
    query = list(
      limit = 1,
      access_token = 1
      #access_token = tidygeocoder:::get_key(selected_method)
    )
  )

raw_results <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

results_minimal <-
  tidygeocoder::extract_results(selected_method, raw_results, full_results = FALSE)

results <-
  tidygeocoder::extract_results(selected_method, raw_results)

full_results_notflat <-
  tidygeocoder::extract_results(selected_method,
    raw_results,
    full_results = TRUE,
    flatten = FALSE
  )
full_results_flat <-
  tidygeocoder::extract_results(selected_method,
    raw_results,
    full_results = TRUE,
    flatten = TRUE
  )

# Test geo ----

## Error
tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  method = "mapbox",
  custom_query = list(
    country = "error"
  )
)

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  method = "mapbox",
  mapbox_permanent = "TRUE"
)


livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "mapbox"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    method = "mapbox"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "mapbox"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::geo(
    address = c("Santiago de Compostela; Spain", "Nieva"),
    verbose = TRUE,
    full_results = TRUE,
    limit = 2,
    custom_query = list(
      country = "ES",
      language = "fr",
      types = "poi,district"
    ),
    method = "mapbox"
  )

glimpse(livetest_params)

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
  geocode(addr, method = "mapbox", lat = latitude, long = longitude)

lat_longs
