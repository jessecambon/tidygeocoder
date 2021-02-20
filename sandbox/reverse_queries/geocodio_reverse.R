selected_method <- 'geocodio'

# https://www.geocod.io/docs/#reverse-geocoding

url_base  <- 'https://api.geocod.io/v1.6/reverse'

# white house
lat <- 38.895865
lon <- -77.0307713

soup <- httr::GET(url = url_base, 
                  query = list(q = paste0(as.character(lat), ',', as.character(lon)), 
                               limit = 1,
                               api_key = tidygeocoder:::get_key(selected_method)))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results <- tidygeocoder::extract_results(selected_method, raw_results)

full_results_notflat <- tidygeocoder::extract_results(selected_method, raw_results, full_results = TRUE, flatten = FALSE)
full_results_flat <- tidygeocoder::extract_results(selected_method, raw_results, full_results = TRUE, flatten = TRUE)



