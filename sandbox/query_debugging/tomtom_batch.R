library(tibble)

library(mapSpain)

addresses <- c("tower bridge","-------","santiago")
addresses <- mapSpain::esp_munic.sf[780:800,]$name

api_url = NULL
verbose = TRUE
custom_query = list(language = "fr-FR")
limit = 1
timeout = 20

# Synchronous ----
# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/synchronous-batch

address_df <-tibble::tibble(address = addresses)


if (is.null(api_url)) api_url <- 'https://api.tomtom.com/search/2/batch.json'

# Construct query
query_parameters <- get_api_query('tomtom', list(limit = limit, api_key = get_key('tomtom')),
                                  custom_parameters = custom_query)
if (verbose == TRUE) display_query(api_url, query_parameters)

# Parameters needs to be included on each element
q_elements <- query_parameters[names(query_parameters) != 'key']

q_string <- ''

for (par in seq_len(length(q_elements))){
  dlm <- if (par==1) '?' else '&'
  q_string <- paste0(q_string, dlm, names(q_elements[par]), '=',  q_elements[[par]])
}

# Construct body
address_list <- list(batchItems = list())

for (index in 1:nrow(address_df)) {
  address_list$batchItems[[index]] <- list(
    query = paste0('/geocode/',as.list(address_df[index,]),'.json', q_string)
    )
}

address_list


# Query API
raw_content <- query_api(api_url, query_parameters, mode = 'list', input_list = address_list, timeout = timeout)


# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(raw_content)

result_list <- content$batchItems$response$results
# if no results are returned for a given address then there is a 0 row dataframe in this
# list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
result_list_filled <- lapply(result_list, filler_df, c('position.lat','position.lng'))

results <- dplyr::bind_rows(result_list_filled)
names(results)

results$lat <- results$position$lat
results$long <- results$position$lon

names(results)[names(results) == 'lat'] <- lat
names(results)[names(results) == 'long'] <- long

# Parse response to get named list with components 'input' and 'results'
dat <- jsonlite::fromJSON(httr::content(res, as = 'text', encoding = "UTF-8"), flatten = TRUE)

# Live test -----
library(tibble)
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  verbose = TRUE)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  limit = 5,
  full_results = TRUE,
  return_addresses = FALSE,
  verbose = TRUE)

ss <- tidygeocoder::geo(
  address = c("Plaza Mayor","xxxxxxxxx" ,"George Street", "-----"),
  method = "tomtom",
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  verbose = TRUE,
  custom_query = list(language = "de-DE"))
glimpse(ss)

# Final test - huge batch - not run, daily limit is 2500 ----
library(mapSpain)
library(tibble)
library(dplyr)

address <- tibble(direcciones = mapSpain::esp_munic.sf$name) %>%
  slice(1:1000)

address_geo <- address %>%
  geocode(address = "direcciones", method = "tomtom", full_results = TRUE, 
          verbose = TRUE)


address_geo