selected_method <- 'google'

# https://developers.google.com/maps/documentation/geocoding/start

url_base  <- 'https://maps.googleapis.com/maps/api/geocode/json'

# white house
lat <- 38.895865
lon <- -77.0307713

soup <- httr::GET(url = url_base, 
                  query = list(latlng = paste0(as.character(lat), ',', as.character(lon)), 
                               key = tidygeocoder:::get_key(selected_method)))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))




address <-  raw_results$results[1,][c('formatted_address')]

results  <- raw_results$results[1,]

combi_results <- dplyr::bind_cols(address, results)
