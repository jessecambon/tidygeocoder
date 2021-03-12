selected_method <- "here"

addr <- "Acueducto de Segovia, Spain"
url_base <- tidygeocoder:::get_here_url()

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url = url_base,
    query = list(
      limit = 1,
      q = addr,
      apiKey = tidygeocoder:::get_key(selected_method)
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))


tidygeocoder::extract_results(selected_method, response, full_results = FALSE)



tidygeocoder::extract_results(selected_method, response)

tidygeocoder::extract_results(selected_method,
  response,
  full_results = TRUE,
  flatten = FALSE
)


tidygeocoder::extract_results(selected_method,
  response,
  full_results = TRUE,
  flatten = TRUE
)

# Test geo ----
library(tibble)
addr <- "Acueducto de Segovia, Spain"
## Error
tidygeocoder::geo(
  address = "zzzzzz",
  verbose = TRUE,
  method = "here",
)


livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "here"
  )
glimpse(livetest)

livetest_full <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    method = "here"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "here"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::geo(
    address = c("Santiago de Compostela; Spain", "Nieva"),
    verbose = TRUE,
    full_results = TRUE,
    mode = 'single',
    limit = 2,
    custom_query = list(
      lang = "fr"
    ),
    method = "here"
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
  geocode(addr, method = "here", mode = 'single', lat = latitude, long = longitude, full_results = TRUE)

lat_longs
