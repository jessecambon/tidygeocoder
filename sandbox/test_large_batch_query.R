# Can use this script for testing large batch queries


library(tidyverse)
library(janitor)

set.seed(42) # reproducibility

raw_louisville <- read_csv(here::here('data-raw/jefferson.zip'))

clean_louisville <- raw_louisville %>%
  janitor::clean_names() %>%
  transmute(street = str_c(number, ' ', street), 
            city = 'Louisville', state = 'Kentucky', postcode, lat, lon) %>%
  rename(latitude = lat, longitude = lon, zip = postcode)

big_louisville <- clean_louisville %>%
  sample_n(1200)

# test batch geocoder

big_louisville_results <- big_louisville %>%
  tidygeocoder::geocode(street = street, city = city, state = state, postalcode = zip, 
                    full_results = TRUE, return_type = 'geographies', verbose = TRUE)

# 1200 addresses (1180 unique) took 166 seconds

# check latitude and longitude and make sure addresses line up
louisville_check <- big_louisville_results %>%
  mutate(
    lat_diff = lat - latitude,
    long_diff = long - longitude
    ) 
