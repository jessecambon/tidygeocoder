# Create small address dataset for testing
# this script creates the .rda file in /data

sample_addresses <- tibble::tribble( ~name,~addr,
                           "White House", "1600 Pennsylvania Ave Washington, DC",
                           "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",
                           NA, "Fake Address",
                           NA,NA,
                           "","",
                           "US City","Nashville,TN",
                           "Willis Tower", "233 S Wacker Dr, Chicago, IL 60606",
                           "International City", "Nairobi, Kenya"
                           )

usethis::use_data(sample_addresses, overwrite = TRUE)
