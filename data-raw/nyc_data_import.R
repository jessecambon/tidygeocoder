# NYC address data downloaded as a zip file 
# from http://results.openaddresses.io/sources/us/ny/city_of_new_york
# on June 1st 2020

library(tidyverse)
library(janitor)

set.seed(42) # reproducibility

raw_nyc <- read_csv('city_of_new_york.csv.tar.xz')

clean_nyc <- raw_nyc %>%
  janitor::clean_names() %>%
  transmute(street = str_c(number, ' ', street), 
    city = 'New York', state = 'New York', postcode, lon, lat) %>%
  rename(latitude = lon, longitude = lon)

nyc_addresses <- clean_nyc %>%
  sample_n(12000)

usethis::use_data(nyc_addresses, overwrite = TRUE)
