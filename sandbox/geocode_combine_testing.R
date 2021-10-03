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

##