# this script creates the .rda file in /data

# Small address dataset for testing
sample_addresses <- tibble::tribble(
  ~name, ~addr,
  "White House", "1600 Pennsylvania Ave NW Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",
  "NY Stock Exchange", "11 Wall Street, New York, New York",
  "Willis Tower", "233 S Wacker Dr, Chicago, IL 60606",
  "Chateau Frontenac", "1 Rue des Carrieres, Quebec, QC G1R 4P5, Canada",
  "Nashville", "Nashville, TN",
  "Nairobi", "Nairobi, Kenya",
  "Istanbul", "Istanbul, Turkey",
  "Tokyo", "Tokyo, Japan",
)


# geocoded with Nominatim (via tidygeocoder) with exception of Chateau Frontenac
# which was geocoded with https://nominatim.openstreetmap.org/
sample_coordinates <- tibble::tribble(
  ~name,  ~lat,  ~long,
  "White House",  38.8976997, -77.03655315,
  "Transamerica Pyramid", 37.79520055, -122.402792678401,
  "NY Stock Exchange",  40.7070653, -74.0111743828446,
  "Willis Tower",  41.8788717, -87.6359078411456,
  "Chateau Frontenac", 46.81182845, -71.20554902764246,
  "Nashville", 36.1622767, -86.7742984,
  "Nairobi", -1.30325275, 36.8263676855587,
  "Istanbul", 41.0091982, 28.9662187,
  "Tokyo",  35.6828387, 139.7594549
)

usethis::use_data(sample_addresses, overwrite = TRUE)
usethis::use_data(sample_coordinates, overwrite = TRUE)
