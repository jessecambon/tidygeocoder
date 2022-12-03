library(dplyr)
library(tibble)

# input dataset from user
# dataset with duplicate addresses and data
inputs <- tribble(
  ~id, ~address, ~descr, ~info,
  1, "Paris", "Capital of France", "",
  1, "Paris", "Population", "2.1 million",
  2, "London", "Capital of UK", "",
  3, "Tokyo", "Capital of Japan", "",
  3, "Tokyo", "Population", "14.0 million"
)

# this is what geocoder could return (multiple results for Paris (ie. limit = 2))
geocoder_service_results <- tribble(
  ~address, ~lat, ~long, ~language, ~country,
  "Paris", 1,1, "French", "France",
  "Paris", 9,9, "English", "United States",
  "London", 2,2, "English", "United Kingdon",
  "Tokyo", 3,3, "Japanese", "Japan"
)

# Assumptions:
  # Single address geocoding

# Steps
#   1: Number the addresses
#   2: Pass unique address to geocoder service (mock this)
#   3: Duplicate the unique geocoder service results (as needed)
#   4: Join the duplicated geocoder service results and the original data together

total_results <- tibble::tibble()
for (address in unique(inputs$address)) {
  # print(paste0('address = ', address)) 
  
  joined <- dplyr::left_join(
    inputs %>% filter(address == !!address), 
    geocoder_service_results, 
    by = 'address')
  
  # print("joined:")
  # print(joined)
  total_results <- dplyr::bind_rows(total_results, joined)
}

#pack <- tidygeocoder:::package_addresses(address = inputs$address, data = inputs %>% select(-address))
#unpack <- tidygeocoder:::unpackage_inputs(pack, geocoder_service_results)
#numbered_addresses <- inputs %>%
  