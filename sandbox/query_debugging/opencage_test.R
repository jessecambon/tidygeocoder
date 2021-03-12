selected_method <- 'opencage'

addr <- '1600 Pennsylvania Ave NW Washington, DC'
url_base  <- tidygeocoder:::get_opencage_url()

library(httr)
library(jsonlite)
library(dplyr)

soup <- httr::GET(url = url_base, 
                  query = list(q = addr, 
                               limit = 1,
                          #     key = 1
                              key = tidygeocoder:::get_key(selected_method)
                  )
                )

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results <- tidygeocoder::extract_results(selected_method, raw_results)

full_results_notflat <- tidygeocoder::extract_results(selected_method, raw_results, full_results = TRUE, flatten = FALSE)
full_results_flat <- tidygeocoder::extract_results(selected_method, raw_results, full_results = TRUE, flatten = TRUE)



