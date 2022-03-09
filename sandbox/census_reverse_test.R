# Census
## https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf

library(httr)
library(jsonlite)
library(dplyr)

x_val <- -111.8681
y_val <- 33.39545

url_base <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates"

# limit=1 limits the query to one result
# benchmark = 'Public_AR_Current'

resp <- httr::GET(url = url_base, 
      query = list(
        x = x_val,
        y = y_val,
        limit = 1,
        format = 'json', 
        benchmark = 'Public_AR_Current', 
        vintage = 'Current_Current')
  )

# dataframe is returned
raw_results <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = "UTF-8"))

final_results <- raw_results$result$geographies

