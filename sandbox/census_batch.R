# Census
## https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf
# https://github.com/slu-openGIS/censusxy/blob/master/R/internal.R

### NOTE: different endpoint urls for US and Europe are available

library(tibble)
library(httr)
library(jsonlite)

url_base <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

return <- 'locations' # match to url used

return_cols <- switch(return,
 'locations' = c('id', 'address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side'),
 'geographies' = c('id', 'address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side', 'state_id', 'county_id', 'tract_id', 'block_id')
)

addresses <- c("1600 Pennsylvania Ave Washington, DC", 
               "2101 Constitution Ave NW, Washington, DC 20418",
               "11 Wall Street, New York, New York",
               "600 Montgomery St, San Francisco, CA 94111",
               "233 S Wacker Dr, Chicago, IL 60606"
               )

num_addresses <- length(addresses)

NA_values <- rep("",num_addresses)

input_df <- tibble(
  id = 1:num_addresses,
  street = addresses,
  city = NA_values,
  state = NA_values,
  zip = NA_values,
)

# Write a Temporary CSV
tmp <- tempfile(fileext = '.csv')
utils::write.table(input_df, tmp, row.names = FALSE, col.names = FALSE, sep = ',', na = '')

check <- read.csv(tmp, header = FALSE)

req <-
  httr::POST(url_base,
       body = list(
         addressFile = httr::upload_file(tmp),
         benchmark = 4,
         format = 'json'
       ),
       encode = 'multipart',
       httr::timeout(60)
  )

cnt <- httr::content(req, as = 'text', encoding = 'UTF-8')

results <- utils::read.csv(text = cnt, header = FALSE,
                      col.names = return_cols,
                      fill = TRUE, stringsAsFactors = FALSE,
                      na.strings = '')

### Extract Coordinates

# extract single coordinate value
extract_coord <- function(input) as.numeric(unlist(strsplit(input,"\\,")))

all_coordinates <- lapply(as.list(results$coords),extract_coord)

coord_df <- do.call('rbind', all_coordinates)
colnames(coord_df) <- c('lat', 'long')

# Combine extracted lat/longs with other return results
combi <- cbind(subset(results, select = -coords),coord_df)

# convert to tibble
df_final <- as_tibble(combi)


#results <- jsonlite::fromJSON(cnt)
