# Throttle easily
# Slow response times

method <- "geocode.xyz"

addr <- "Toronto"
url_base <- tidygeocoder:::get_geocodexyz_url()

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url =url_base,
    query = list(
      locate = addr,
      geoit = "json"
    )
  )


response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))


results_minimal <-
  tidygeocoder::extract_results(method, response, full_results = FALSE)

results_minimal

results <-
  tidygeocoder::extract_results(method, response)

results

full_results_notflat <-
  tidygeocoder::extract_results(method,
                                response,
                                full_results = TRUE,
                                flatten = FALSE
  )
full_results_notflat

full_results_flat <-
  tidygeocoder::extract_results(method,
                                response,
                                full_results = TRUE,
                                flatten = TRUE
  )
full_results_flat


# Error on bad auth

response <- tidygeocoder:::query_api(
  api_url = url_base,
  query_parameters = list(
    locate = addr,
    geoit = "json",
    auth="12345"
  )
)

raw_results <- extract_errors_from_results(method = "geocode.xyz",response$content)

# No error on no results

response2 <- tidygeocoder:::query_api(
  api_url = url_base,
  query_parameters = list(
    locate = "insnois29j00'2",
    geoit = "json"
  )
)

raw_results <- jsonlite::fromJSON(response2$content)

# No error but is blank
extract_errors_from_results(method = "geocode.xyz",response2$content)

extract_results("geocode.xyz", response2)

# Test geo ----
library(tibble)
addr <- "Plaza Mayor"

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "geocode.xyz",
)

# With params
tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "geocode.xyz",
  full_results = TRUE,
  limit = 5,
  custom_query =  list(region="ES")
)

livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "geocode.xyz"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::geo(
    address = "Antonio de Leyva, Madrid",
    verbose = TRUE,
    full_results = TRUE,
    method = "geocode.xyz"
  )
glimpse(livetest_full)


livetest_params <-
  tidygeocoder::geo(
    address = c("Santiago de Compostela; Spain", "Nieva"),
    verbose = TRUE,
    full_results = TRUE,
    mode = "single",
    limit = 2,
    custom_query = list(
      moreinfo = 1
    ),
    method = "geocode.xyz"
  )

glimpse(livetest_params)

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
          method = "geocode.xyz", lat = latitude, long = longitude,
          full_results = TRUE, mode = "single", verbose = TRUE
  )

lat_longs
