# Google query
# needs GOOGLEGEOCODE_API_KEY in .Renviron
# developer documentation https://developers.google.com/maps/documentation/geocoding/overview

addr <- 'Tokyo, Japan'
url_base  <- "https://maps.googleapis.com/maps/api/geocode/json"

library(httr)
library(jsonlite)
library(dplyr)

soup <- httr::GET(url = url_base, 
                  query = list(address = addr, 
                               key = tidygeocoder:::get_key("google")))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results <- tidygeocoder::extract_results('google', raw_results, full_results = FALSE)

full_results_notflat <- tidygeocoder::extract_results('google', raw_results, full_results = TRUE, flatten = FALSE)
full_results_flat <- tidygeocoder::extract_results('google', raw_results, full_results = TRUE, flatten = TRUE)



