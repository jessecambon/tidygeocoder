
### Problem : this returns NA lat/long due to extract_results
# a <- tidygeocoder::geo("Shop A & B, GF Coble Court, 127 – 139 Ap Lei Chau Main Street, Aberdeen, HK", method = "google",
# verbose = TRUE, limit = 2, full_results = TRUE)

addr <- "Shop A & B, GF Coble Court, 127 – 139 Ap Lei Chau Main Street, Aberdeen, HK"
url_base  <- "https://maps.googleapis.com/maps/api/geocode/json"

library(httr)
library(jsonlite)
library(dplyr)

soup <- httr::GET(url = url_base, 
                  query = list(address = addr, 
                               key = tidygeocoder:::get_key("google")))

response <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))



results <- tidygeocoder::extract_results('google', response, full_results = FALSE, limit = 2)


lat_lng <- tibble::as_tibble(response$results$geometry$location[c('lat','lng')][1:2, ])
