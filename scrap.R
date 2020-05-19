# OSM query

# https://nominatim.org/release-docs/develop/api/Search/

server <- "http://nominatim.openstreetmap.org"

library(httr)
library(jsonlite)


server="http://nominatim.openstreetmap.org"
addr <- '20 Downing St London'
query <- gsub(" ", "+", enc2utf8(addr), fixed = TRUE)

url <- paste0(server, "/search?q=", query, 
              "&format=json&polygon=0&addressdetails=0")

soup <- httr::GET(url = url)
dat <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"), simplifyVector = TRUE)
