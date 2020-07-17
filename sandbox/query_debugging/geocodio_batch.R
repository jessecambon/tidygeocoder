# Geocodio
## https://www.geocod.io/docs/#geocoding
#  (obtain via personal account on https://www.geocod.io)
# set api key with Sys.setenv(GEOCODIO_API_KEY="")
# To permanently set, add to you .Renviron file in your HOME directory

desired_format <- '{
  "1": {
    "street": "1109 N Highland St",
    "city": "Arlington",
    "state": "VA"
  },
  "2": {
    "city": "Toronto",
    "country": "CA"
  }
}'

r_format <- jsonlite::fromJSON(desired_format)

geocodio_api_key <- Sys.getenv("GEOCODIO_API_KEY")

library(httr)
library(jsonlite)
library(tibble)
url_base <- "https://api.geocod.io/v1.6/geocode"

### single line addresses
# addresses <- c("1600 Pennsylvania Ave Washington, DC", "2101 Constitution Ave
# NW, Washington, DC 20418", "11 Wall Street, New York, New York", "600
# Montgomery St, San Francisco, CA 94111", "233 S Wacker Dr, Chicago, IL 60606",
# '1 Rue des Carrières, Québec, QC G1R 4P5, Canada' )

addresses <- c('Tokyo', 'Tokyo1')

# limit=1 limits the query to one result
res <- httr::POST(url_base,
                  query = list(api_key = geocodio_api_key, limit = 1), 
                  body = addresses, encode = 'json')

httr::warn_for_status(res)

# Parse response to get named list with components 'input' and 'results'
dat <- jsonlite::fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"), flatten = TRUE)

extract_results <- function(x) {
  return(x$response$results)
}

agg_results <- lapply(dat$results,extract_results)

#x_fill <- lapply(x, filler_df, c('location'))

combi <- dplyr::bind_rows(agg_results)
