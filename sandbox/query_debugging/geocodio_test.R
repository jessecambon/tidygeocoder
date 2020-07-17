# Geocodio
## https://www.geocod.io/docs/#geocoding
# geocodio_api_key <- manually defined (obtain via personal account on https://www.geocod.io)

geocodio_api_key <- Sys.getenv("GEOCODIO_API_KEY")

library(httr)
library(jsonlite)

addr <- 'Tokyo' # address to geocode

url_base <- "https://api.geocod.io/v1.6/geocode"

# limit=1 limits the query to one result

# Geocodio returns json by default
resp <- httr::GET(url = url_base, 
      query = list(
        q = addr,
        api_key = geocodio_api_key,
        limit = 1
        ))

# Parse response to get named list with components 'input' and 'results'
response <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = "UTF-8"))

product <- response$results[!names(response$results) %in% c('location')]


#lat_lng <- c(dat$results$location$lat, dat$results$location$lng)
