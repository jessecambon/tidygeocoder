# Geocodio
## https://www.geocod.io/docs/#geocoding
#  (obtain via personal account on https://www.geocod.io)
# set api key with Sys.setenv(GEOCODIO_API_KEY="")

geocodio_api_key <- Sys.getenv("GEOCODIO_API_KEY")

library(httr)
library(jsonlite)
url_base <- "https://api.geocod.io/v1.5/geocode"


addresses <- c("1600 Pennsylvania Ave Washington, DC", 
               "2101 Constitution Ave NW, Washington, DC 20418",
               "11 Wall Street, New York, New York",
               "600 Montgomery St, San Francisco, CA 94111",
               "233 S Wacker Dr, Chicago, IL 60606",
               '1 Rue des Carrières, Québec, QC G1R 4P5, Canada' 
)

# limit=1 limits the query to one result
res <- httr::POST(url_base,
                  query = list(api_key = geocodio_api_key), 
                  body = as.list(addresses), encode = "json")


# Parse response to get named list with components 'input' and 'results'
dat <- jsonlite::fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"), flatten = TRUE)

# 
results <- dat$results

x <- results$response.results
