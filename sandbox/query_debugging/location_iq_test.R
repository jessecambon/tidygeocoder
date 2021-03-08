# Location IQ
## https://locationiq.com/docs#forward-geocoding
# iq_api_key <- manually defined (obtain via personal account on https://my.locationiq.com/)

iq_api_key <- Sys.getenv("LOCATIONIQ_API_KEY")
  
### NOTE: different endpoint urls for US and Europe are available

library(httr)
library(jsonlite)

addr <- '1 Rue des Carrières, Québec, QC G1R 4P5, Canada' # address to geocode

url_base <- "https://us1.locationiq.com/v1/search.php"

# limit=1 limits the query to one result

resp <- httr::GET(url = url_base, 
      query = list(
        q = addr,
        format = 'json',
        key = iq_api_key,
        limit = 1
        ))

# dataframe is returned
raw_results <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = "UTF-8"))

# Obtain latitude and longitude
lat_lng <- as.numeric(c(dat$lat, dat$lon))

