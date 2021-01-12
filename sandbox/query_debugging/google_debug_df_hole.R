# Google query
# needs GOOGLEGEOCODE_API_KEY in .Renviron
# developer documentation https://developers.google.com/maps/documentation/geocoding/overview

prob_addr <- tibble(address = c('11 Wall St New York, NY', NA, 
                                '1600 Pennsylvania Ave NW Washington, DC',
                                NA, '11 Wall St New York, NY'))

results <- tibble(lat = c(tibble(y=1), NA))

pack <- tidygeocoder:::package_addresses(address = prob_addr$address)

unpack <- tidygeocoder:::unpackage_addresses(pack, results, return_addresses = TRUE)


