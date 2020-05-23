# OSM query

addr <- 'Tokyo, Japan'

# https://nominatim.org/release-docs/develop/api/Search/
url_base <- "http://nominatim.openstreetmap.org/search"

library(httr)
library(jsonlite)

# limit =1 limits query to one result

soup <- httr::GET(url = url_base, 
                  query = list(q = addr, format = 'json',limit = '1'))

dat <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"), simplifyVector = TRUE)

# Get lat/long coordinates
coords <- as.numeric( c(dat$lat, dat$lon) )
