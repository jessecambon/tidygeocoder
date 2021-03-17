selected_method <- "arcgis"

addr <- "Acueducto de Segovia, Spain"
url_base <- tidygeocoder:::get_arcgis_url()

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url =url_base,
    query = list(
      f = "json",
      maxLocations = 5,
      SingleLine = addr
    )
  )

raw_results <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

raw_results$candidates$location[c('y', 'x')]


results_minimal <-
  tidygeocoder::extract_results(selected_method, raw_results, full_results = FALSE)

results_minimal

results <-
  tidygeocoder::extract_results(selected_method, raw_results)

results

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

# Error on bad user

response <- tidygeocoder:::query_api(
  api_url = url_base,
  query_parameters = list(
    f = "json",
    maxLocations = 5,
    SingleLine = addr,
    token="aa"
  )
)
response$content

# Error on bad key


# Test geo ----
library(tibble)
addr <- "Plaza Mayor"

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "arcgis",
  limit = 5,
)
s2 <- tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "arcgis",
  full_results = TRUE,
  limit = 5,
  custom_query =  list(outFields="*")
)

livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "arcgis"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::geo(
    address = "Antonio de Leyva, Madrid",
    verbose = TRUE,
    full_results = TRUE,
    method = "arcgis"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "arcgis"
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
    method = "arcgis"
  )

glimpse(livetest_params)
# Error on format
tidygeocoder::geo(
  address = c("Nieva"),
  verbose = TRUE,
  full_results = TRUE,
  mode = "single",
  limit = 2,
  custom_query = list(
    f = "aaaaaa"
  ),
  method = "arcgis"
)

tidygeocoder::geo(
  address = c("Nieva"),
  verbose = TRUE,
  full_results = TRUE,
  mode = "single",
  limit = 2,
  custom_query = list(
    forStorage = "aaaaaa"
  ),
  method = "arcgis"
)

tidygeocoder::geo(
  address = "New York, USA", method = "arcgis",
  custom_query = list(
    token = "<API_KEY>"
  )
)


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
          method = "arcgis", lat = latitude, long = longitude,
          full_results = TRUE, mode = "single", verbose = TRUE
  )

lat_longs
