# OSM query

addr <- 'Tokyo, Japan'

# https://nominatim.org/release-docs/develop/api/Search/

url_base <- "http://nominatim.openstreetmap.org/search"

library(httr)
library(jsonlite)

#server="http://nominatim.openstreetmap.org"

#query <- gsub(" ", "+", enc2utf8(addr), fixed = TRUE)
#url <- paste0(server, "/search?q=", query, 
#              "&format=json&polygon=0&addressdetails=0")

# limit =1 limits query to one result

soup <- httr::GET(url = url_base, 
                  query = list(q = addr, format = 'json',limit=1))

#soup <- httr::GET(url = url)
dat <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"), simplifyVector = TRUE)

coords <- as.numeric( c(dat$lat, dat$lon) )
