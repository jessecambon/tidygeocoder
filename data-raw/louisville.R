# Louisville address data downloaded as a zip file
# from http://results.openaddresses.io/sources/us/ky/jefferson
# on June 1st 2020

library(tidyverse)
library(janitor)

set.seed(42) # reproducibility

raw_louisville <- read_csv("jefferson.zip")

clean_louisville <- raw_louisville %>%
  janitor::clean_names() %>%
  transmute(
    street = str_c(number, " ", street),
    city = "Louisville", state = "Kentucky", postcode, lat, lon
  ) %>%
  rename(latitude = lat, longitude = lon, zip = postcode)

louisville <- clean_louisville %>%
  sample_n(50)

usethis::use_data(louisville, overwrite = TRUE)
