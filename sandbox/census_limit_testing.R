# Finding: geocode() mixes up results when limit > 1 for census single geocoding
# But geo() works fine
#
#

library(tidygeocoder)
library(dplyr)

tie_addresses <- tibble::tribble(
  ~res_street_address, ~res_city_desc, ~state_cd, ~zip_code,
  "624 W DAVIS ST   #1D",   "BURLINGTON",      "NC",     27215,
  "201 E CENTER ST   #268",       "MEBANE",      "NC",     27302,
  "7833  WOLFE LN",    "SNOW CAMP",      "NC",     27349,
)

## Try using tidygeocoder single address geocoding - results are returned
tg_single <- geo(street = tie_addresses$res_street_address,
          city = tie_addresses$res_city_desc,
          state = tie_addresses$state_cd,
          postalcode = tie_addresses$zip_code,
          method = 'census', 
          limit = 5,
          mode = 'single',
          full_results = TRUE, 
          return_type = 'geographies'
  )

tg_single_geocode <- tie_addresses %>%
  geocode(street = res_street_address,
          city = res_city_desc,
          state = state_cd,
          postalcode = zip_code,
          method = 'census', 
          mode = 'single',
          limit = 2,
          full_results = TRUE, 
          return_type = 'geographies'
  )

library(tidygeocoder)
library(dplyr)
tibble(address = c('Tokyo', 'Paris', 'Rome')) %>%
  geocode(address, method = 'osm', limit = 3, full_results = TRUE) %>%
  select(address, display_name)
  
