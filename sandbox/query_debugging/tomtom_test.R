selected_method <- "tomtom"

addr <- "Acueducto de Segovia, Spain"
url_base <- tidygeocoder:::get_tomtom_url()
grep('tomtom',"url_base")
library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url = gsub(" ", "%20", paste0(url_base, addr, ".json")),
    query = list(
      limit = 1,
      key = tidygeocoder:::get_key(selected_method)
    )
  )

raw_results <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

is.data.frame(raw_results$results)
httr::content(soup, as = "text", encoding = "UTF-8")

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
  api_url = gsub(" ", "%20", paste0(url_base, addr, ".json")),
    query_parameters =  list(
      limit = 1,
      key = tidygeocoder:::get_key(selected_method),
      language = "FFFFF"
    )
  )
raw_results <- jsonlite::fromJSON(response$content)

httr::status_code(response)
httr::warn_for_status(response)
httr::content(response, as = 'text', encoding = "UTF-8")
content <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = "UTF-8"))

httr::content(response, as = 'text', encoding = "UTF-8")

tidygeocoder::check_results_for_problems("tomtom", soup2, TRUE)
# Error on bad key


# Test geo ----
library(tibble)
addr <- "Plaza Mayor"

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "tomtom",
  limit = 5,
)
tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "tomtom",
  limit = 5,
  custom_query =  list(key="aa")
)

livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "tomtom"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::geo(
    address = "Antonio de Leyva, Madrid",
    verbose = TRUE,
    full_results = TRUE,
    method = "tomtom"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "tomtom"
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
    method = "tomtom"
  )

# Error
tidygeocoder::geo(
  address = c("Nieva"),
  verbose = TRUE,
  full_results = TRUE,
  mode = "single",
  limit = 2,
  custom_query = list(
    language = "aaaaaa"
  ),
  method = "tomtom"
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
  geocode(addr,
    method = "tomtom", lat = latitude, long = longitude,
    full_results = TRUE, mode = "single", verbose = TRUE
  )

lat_longs
