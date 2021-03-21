library(tidygeocoder)
library(dplyr)
library(httr)
library(jsonlite)

# Also found ties don't produce coordinates:
# https://freestok.github.io/geocode.html

tie_addresses <- tibble::tribble(
  ~res_street_address, ~res_city_desc, ~state_cd, ~zip_code,
  "624 W DAVIS ST   #1D",   "BURLINGTON",      "NC",     27215,
  "201 E CENTER ST   #268",       "MEBANE",      "NC",     27302,
  "7833  WOLFE LN",    "SNOW CAMP",      "NC",     27349,
)

## Try using tidygeocoder batch --- NA return
tg_batch <- tie_addresses %>%
  geocode(street = res_street_address,
          city = res_city_desc,
          state = state_cd,
          postalcode = zip_code,
          method = 'census', 
          full_results = TRUE, 
          return_type = 'geographies'
  )

## Try using tidygeocoder single address geocoding - results are returned
tg_single <- tie_addresses %>%
  geocode(street = res_street_address,
          city = res_city_desc,
          state = state_cd,
          postalcode = zip_code,
          method = 'census', 
          mode = 'single',
          full_results = TRUE, 
          return_type = 'geographies'
  )

### -----------------------------------------------------------------------
## Try manually performing the census batch query: 

# geographies or locations
return <- 'geographies'

url_base <- tidygeocoder:::get_census_url(return, 'addressbatch')

# column names for what the census batch geocoder returns
# https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html

location_cols <- c('id', 'input_address', 'match_indicator', 'match_type', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side')
return_cols <- switch(return,
                      'locations' = location_cols,
                      'geographies' = c(location_cols, c('state_fips', 'county_fips', 'census_tract', 'census_block'))
)

input_df <- tibble(
  id = 1:nrow(tie_addresses),
  street = tie_addresses$res_street_address,
  city = tie_addresses$res_city_desc,
  state = tie_addresses$state_cd,
  zip = tie_addresses$zip_code,
)

# Write a Temporary CSV
tmp <- tempfile(fileext = '.csv')
utils::write.table(input_df, tmp, row.names = FALSE, 
                   col.names = FALSE, sep = ',', na = '', 
                   qmethod = 'double', fileEncoding = "UTF-8")

check <- read.table(tmp, header = FALSE, na.strings = '', sep = ',')

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
