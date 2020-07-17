# Census
## https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf
# https://github.com/slu-openGIS/censusxy/blob/master/R/internal.R

### NOTE: different endpoint urls for US and Europe are available

library(tibble)
library(httr)
library(jsonlite)

url_base <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

return <- 'locations' # match to url used

# column names for what the census batch geocoder returns
# https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html

location_cols <- c('id', 'input_address', 'match_indicator', 'match_type', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side')
return_cols <- switch(return,
 'locations' = location_cols,
 'geographies' = c(location_cols, c('state_fips', 'county_fips', 'census_tract', 'census_block'))
)

# addresses <- c("1600 Pennsylvania Ave Washington, DC", 
#                "2101 Constitution Ave NW, Washington, DC 20418",
#                "11 Wall Street, New York, New York",
#                "600 Montgomery St, San Francisco, CA 94111",
#                "233 S Wacker Dr, Chicago, IL 60606"
#                )
#addresses <- as.vector(sample_addresses$addr)[c(1:3,5)]

#num_addresses <- length(addresses)

#NA_values <- rep("",num_addresses)

input_df <- tibble(
  id = 1:50,
  street = louisville$street,
  city = louisville$city,
  state = louisville$state,
  zip = louisville$zip,
)

# Write a Temporary CSV
tmp <- tempfile(fileext = '.csv')
utils::write.table(input_df, tmp, row.names = FALSE, 
                   col.names = FALSE, sep = ',', na = '', 
                   qmethod = 'double', fileEncoding = "UTF-8")

check <- read.table(tmp, header = FALSE,na.strings = '',sep = ',')

req <-
  httr::POST(url_base,
       body = list(
         addressFile = httr::upload_file(tmp),
         benchmark = 'Public_AR_Current',
         format = 'json',
         vintage = 'Current_Current'
       ),
       encode = 'multipart',
       httr::timeout(60),
       httr::verbose()
  )

# Note - encoding is important if there are UTF-8 characters passed
# without specifying the encoding here, it will return NA for the whole batch
cnt <- httr::content(req, as = 'text', encoding = "ISO-8859-1")

results <- utils::read.csv(text = cnt, header = FALSE,
                      col.names = return_cols,
                      fill = TRUE, stringsAsFactors = FALSE,
                      na.strings = '')

# reorder by id column
results <- results[order(results['id']), ] 

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
