# Census
## https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf

library(httr)
library(jsonlite)
library(dplyr)

# x_val <- -111.8681
# y_val <- 33.39545

# y_val <- "46.81182845"
# x_val <- "-71.2055490276425"

y_val <- "35.6828387"
x_val <- "139.7594549"

return_type <- 'geographies' # return_type must be geographies for census reverse geocoding
search <- 'coordinates'


url_base <- paste0("https://geocoding.geo.census.gov/geocoder/", return_type, "/", search)

# limit=1 limits the query to one result
# benchmark = 'Public_AR_Current'

resp <- httr::GET(url = url_base, 
      query = list(
        x = x_val,
        y = y_val,
      #  limit = 1,  # limit not used?
        format = 'json', 
        benchmark = 'Public_AR_Current', 
        vintage = 'Current_Current')
  )

# dataframe is returned
raw_results <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = "UTF-8"))

final_results <- raw_results$result$geographies

flattened_final_results <- bind_cols(
  tibble(geography = names(final_results)),
  bind_rows(final_results)
)

