selected_method <- "bing"

addr <- "London, UK"
url_base <- tidygeocoder:::get_bing_url()

library(httr)
library(jsonlite)
library(dplyr)

# Test sandbox on dev ----
soup <-
  httr::GET(
    url = url_base,
    query = list(
      key = tidygeocoder:::get_key('bing'),
      q = addr,
      maxResults = 1
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))


httr::status_code(soup)
length(response$resourceSets$resources[[1]]$point$coordinates)
is.null(response$resourceSets$resources[[1]])
as.data.frame(
  matrix(unlist(response$resourceSets$resources[[1]]$point$coordinates), ncol = 2, byrow = TRUE),
  col.names = c("lat", "lng")
)

results_minimal <-
  tidygeocoder::extract_results(selected_method, response, full_results = FALSE)

results_minimal
results <-
  tidygeocoder::extract_results(selected_method, response)

results

full_results_notflat <-
  tidygeocoder::extract_results(selected_method,
    response,
    full_results = TRUE,
    flatten = FALSE
  )
full_results_notflat

full_results_flat <-
  tidygeocoder::extract_results(selected_method,
    response,
    full_results = TRUE,
    flatten = TRUE
  )
full_results_flat

# Error on bad user
query_results <- query_api(url_base, list(
  key = tidygeocoder:::get_key(selected_method),
  q = "Madrid, Spain",
  strictMatch = "XX"
))


if (TRUE == TRUE) message(paste0("HTTP Status Code: ", as.character(query_results$status)))


tidygeocoder:::extract_errors_from_results(query_results$content, method = "bing")

httr::status_code(response)
httr::warn_for_status(response)
httr::content(response, as = "text", encoding = "UTF-8")
content <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

httr::content(response, as = "text", encoding = "UTF-8")

tidygeocoder::check_results_for_problems("bing", soup2, TRUE)
# Error on bad key


# Test geo ----
library(tibble)
addr <- "Plaza Mayor"

tidygeocoder::geo(
  address = addr,
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "bing",
  limit = 5,
)

# NUll result
tidygeocoder::geo(
  address = "xxxxxxxxxxxxxxxxxxxx",
  verbose = TRUE,
  lat = "latitude",
  long = "longitude",
  method = "bing",
  limit = 5,
)

livetest <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    method = "bing"
  )
glimpse(livetest)
livetest_full <-
  tidygeocoder::geo(
    address = "Antonio de Leyva, Madrid",
    verbose = TRUE,
    full_results = TRUE,
    method = "bing"
  )
glimpse(livetest_full)

livetest_fullflat <-
  tidygeocoder::geo(
    address = addr,
    verbose = TRUE,
    full_results = TRUE,
    flatten = TRUE,
    method = "bing"
  )
glimpse(livetest_fullflat)


livetest_params <-
  tidygeocoder::geo(
    address = c("Madrid"),
    verbose = TRUE,
    full_results = TRUE,
    mode = "single",
    limit = 5,
    method = "bing"
  )

glimpse(livetest_params)

# Error
tidygeocoder::geo(
  address = c("Nieva"),
  verbose = TRUE,
  full_results = TRUE,
  mode = "single",
  limit = 2,
  custom_query = list(
    key = "aaaaaa"
  ),
  method = "bing"
)

tidygeocoder::geo(
  address = c("Nieva"),
  verbose = TRUE,
  full_results = TRUE,
  mode = "single",
  limit = 2,
  custom_query = list(
    strictMatch = "aaaaaa"
  ),
  method = "bing"
)

# End error

glimpse(livetest_params)

library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
  ~name1, ~addr,
  "White House", "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",
  "Willis Tower", "233 S Wacker Dr, Chicago, IL 60606"
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr,
    method = "bing",
    full_results = TRUE, mode = "single", verbose = TRUE
  )

lat_longs
