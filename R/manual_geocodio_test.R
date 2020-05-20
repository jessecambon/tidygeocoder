# Geocodio
## https://www.geocod.io/docs/#geocoding
# geocodio_api_key <- manually defined (obtain via personal account on https://www.geocod.io)


library(httr)
library(jsonlite)

addr <- '1 Rue des Carrières, Québec, QC G1R 4P5, Canada' # address to geocode

url_base <- "https://api.geocod.io/v1.5/geocode"

# limit=1 limits the query to one result

# Geocodio returns json by default
resp <- httr::GET(url = url_base, 
      query = list(
        q = addr,
        api_key = geocodio_api_key,
        limit = 1
        ))

# Parse response to get named list with components 'input' and 'results'
dat <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = "UTF-8"), simplifyVector = TRUE)

# 
results <- dat$results


lat_lng <- c(results$location$lat, results$location$lng)

