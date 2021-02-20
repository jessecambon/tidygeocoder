selected_method <- 'opencage'

# https://opencagedata.com/api#reverse-resp

url_base  <- tidygeocoder:::get_opencage_url()

lat <- 6.455027
lon <- 3.384082

soup <- httr::GET(url = url_base, 
                  query = list(q = paste0(as.character(lat), ',', as.character(lon)), 
                               limit = 1,
                               key = tidygeocoder:::get_key(selected_method)))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results <- tidygeocoder::extract_results(selected_method, raw_results)

full_results_notflat <- tidygeocoder::extract_results(selected_method, raw_results, full_results = TRUE, flatten = FALSE)
full_results_flat <- tidygeocoder::extract_results(selected_method, raw_results, full_results = TRUE, flatten = TRUE)



