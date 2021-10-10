library(tibble)
library(dplyr)
library(tidygeocoder)

## Tie addresses
# For background see this issue: https://github.com/jessecambon/tidygeocoder/issues/87

tie_addresses <- tribble(
  ~res_street_address, ~res_city_desc, ~state_cd, ~zip_code,
  "624 W DAVIS ST   #1D",   "BURLINGTON",      "NC",     27215,
  "201 E CENTER ST   #268",       "MEBANE",      "NC",     27302,
  "100 Wall Street",    "New York",      "NY",     NA
)

tie_results <- tie_addresses %>%
  geocode_combine(
    queries = list(
      list(method = 'census', mode = 'batch'),
      list(method = 'census', mode = 'single')
    ),
    global_params = list(street = 'res_street_address', 
                         city = 'res_city_desc', state = 'state_cd', postalcode = 'zip_code'),
    query_names = c('census batch', 'census single')
  )

## Test sorting (ie. should be returned in the same order as given and no -extra- duplicates)
geo_combine(list(list(method = 'census'), list(method = 'osm')), 
            global_params = list(address = 'address', limit = 1), 
            address = c('Paris', 'Tokyo', 'Paris', '100 Main Street New York, NY', 'London'))

## More complicated query:

geo_combine(list(list(method = 'census'), list(method = 'osm')), 
            global_params = list(address = 'address', limit = 3, mode = 'single', return_input = FALSE, unique_only = TRUE), 
            address = c('Paris', 'Tokyo', 'Paris', '100 Main Street New York, NY', 'London'), cascade = FALSE, return_list = TRUE)

# Does this return NAs properly?

geocode_combine(sample_addresses, list(list(method = 'osm'), list(method = 'arcgis')), 
                global_params = list(address = 'addr', no_query = TRUE))
