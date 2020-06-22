# this script creates the .rda file in /data

# Small address dataset for testing
sample_addresses <- tibble::tribble( 
  ~name,                         ~addr,
   "White House",                "1600 Pennsylvania Ave Washington, DC",
   "Transamerica Pyramid",       "600 Montgomery St, San Francisco, CA 94111",
   "NY Stock Exchange",          "11 Wall Street, New York, New York",
   "Willis Tower",               "233 S Wacker Dr, Chicago, IL 60606",
   "Château Frontenac",          "1 Rue des Carrières, Québec, QC G1R 4P5, Canada",  
   "Nashville",                  "Nashville, TN" ,
   "Nairobi",                    "Nairobi, Kenya",
   "Istanbul",                   "Istanbul, Turkey",
   "Tokyo",                      "Tokyo, Japan",
)

# Invalid/blank addresses for testing ---- move to tests folder
junk_addresses <- tibble::tribble( 
  ~name,               ~addr,
  'NA',                NA,
  'Null',              NULL,
  "Blank",             "",
  "White Space",       "      ",
)

usethis::use_data(sample_addresses, overwrite = TRUE)
#usethis::use_data(invalid_addresses, overwrite = TRUE)
