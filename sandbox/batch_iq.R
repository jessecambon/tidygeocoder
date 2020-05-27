# Location IQ
## https://locationiq.com/docs#forward-geocoding
# iq_api_key <- manually defined (obtain via personal account on https://my.locationiq.com/)
# set api key with Sys.setenv(LOCATIONIQ_API_KEY="")
# To permanently set, add to you .Renviron file in your HOME directory

iq_api_key <- Sys.getenv("LOCATIONIQ_API_KEY")

library(httr)
library(jsonlite)
url_base <- "https://us1.locationiq.com/v1/search.php"


addresses <- c("1600 Pennsylvania Ave Washington, DC", 
               "2101 Constitution Ave NW, Washington, DC 20418",
               "11 Wall Street, New York, New York",
               "600 Montgomery St, San Francisco, CA 94111",
               "233 S Wacker Dr, Chicago, IL 60606",
               '1 Rue des Carrières, Québec, QC G1R 4P5, Canada' 
)

# limit=1 limits the query to one result
res <- httr::POST(url_base,
                  query = list(api_key = iq_api_key, format = 'json'), 
                  body = as.list(addresses), encode = "json")

httr::warn_for_status(res)

# Parse response to get named list with components 'input' and 'results'
dat <- jsonlite::fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"), flatten = TRUE)

# 
results <- dat$results

x <- results$response.results
