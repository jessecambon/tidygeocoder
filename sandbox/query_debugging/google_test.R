# Google query
# needs GOOGLEGEOCODE_API_KEY in .Renviron
# developer documentation https://developers.google.com/maps/documentation/geocoding/overview

addr <- 'Tokyo, Japan'

url_base  <- "https://maps.googleapis.com/maps/api/geocode/json"

library(httr)
library(jsonlite)

get_key("google")

soup <- httr::GET(url = url_base, 
                  query = list(address = addr, 
                               key = get_key('google')))
#,format = 'json'))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results <- extract_results('google', raw_results)

results <- extract_results('google', raw_results, full_results = FALSE)

########## test single values

devtools::load_all()

addr <- 'Tokyo, Japan'
geo(address = addr, method = "osm", verbose = TRUE)
geo(address = addr, method = "google", verbose = TRUE)
geo_google(address = addr)

# fails: google only takes single-line addresses
geo(method = "google", 
    verbose = TRUE,
    street = "24 Sussex Drive",
    city = "Ottawa",
    country = "Canada")

########## test columns in tibble

addresses <- tibble::tibble(addrs = c("Tokyo, Japan", "Toronto, Ontario"))

addresses %>%
  geocode(addrs, method = 'google', verbose = TRUE)

addresses %>%
  geocode(addrs, method = 'google', verbose = TRUE, min_time = 1)


addresses %>%
  geocode(addrs, method = 'osm', verbose = TRUE)


##################
