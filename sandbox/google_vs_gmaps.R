library(tidygeocoder)


service <- "google"

generic_query <- list(address = "Rr. Ukshin Hoti nr. 124 , 10000 Prishtinë, Kosovo")

# add api key, defaults, etc.
generic_query2 <- tidygeocoder:::add_common_generic_parameters(generic_query, service, FALSE, limit = 10)

# get api specific query
query_parameters <- get_api_query(
  service, generic_query2
)

api_url <- tidygeocoder:::get_google_url()

# execute the query
raw <- query_api(
  api_url,
  query_parameters
)

results <- extract_results(service, jsonlite::fromJSON(raw$content))