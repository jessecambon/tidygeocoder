library(tibble)
library(mapSpain)

unique_addresses <- c("tower bridge", "-------", "santiago")
unique_addresses <- tibble::tibble(address=unique_addresses)



api_url <- NULL
verbose <- TRUE
custom_query <- list(language = "fr-aaaa",
                     waitTimeSeconds = 5)
limit <- 1
timeout <- 20
lat <- "lat"
long <- "long"

#unique_addresses <- tibble::tibble(address=mapSpain::esp_get_prov()$ine.prov.name)


# limit the dataframe to legitimate arguments
address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters('tomtom', address_only = TRUE)]

NA_value <- get_na_value(lat, long, rows = nrow(address_df)) # filler result to return if needed



if (is.null(api_url)) api_url <- 'https://api.tomtom.com/search/2/batch.json'

# Construct query - for display only
query_parameters <- get_api_query('tomtom',
                                  list(limit = limit, api_key = get_key('tomtom')),
                                  custom_parameters = custom_query)

if (verbose == TRUE) display_query(api_url, query_parameters)

# Some parameters needs to be included on each element
# Others (key, etc) should be in the query
api_query_params <- query_parameters[names(query_parameters) %in% c('key', 'redirectMode', 'waitTimeSeconds')]
q_elements <- query_parameters[!names(query_parameters) %in% c('key', 'redirectMode', 'waitTimeSeconds')]

q_string <- ''
for (par in seq_len(length(q_elements))) {
  dlm <- if (par == 1) '?' else '&'
  q_string <- paste0(q_string, dlm, 
                     names(q_elements[par]), '=',  q_elements[[par]])
}

# Construct body
address_list <- list(batchItems = list())

for (index in 1:nrow(address_df)) {
  address_list$batchItems[[index]] <- list(query = paste0('/geocode/', as.list(address_df[index, ]), '.json', q_string))
}


# Query API
response <- httr::POST(api_url, query = api_query_params, 
                       body = as.list(address_list), 
                       encode = 'json', httr::timeout(60 * timeout))


if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(response$status_code)))

## Extract results -----------------------------------------------------------------------------------
# if there were problems with the results then return NA
if (!httr::status_code(response) %in% c(200, 202, 303)) {
  extract_errors_from_results('tomtom', httr::content(response), verbose)
  results <- NA_value
}


# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/asynchronous-batch-submission#response-data
# if status code is not 200 we have to perform a GET and download the batch asynchronously
# On 200 the batch is provided in the response object
if (httr::status_code(response) != '200') {
  if (verbose) message('Asynchronous Batch Download')
  
  # A HTTP Response with a Location header that points where the batch results can be obtained.
  query_results$content
  
  status <- httr::status_code(response)
  while (status %in% c('202', '303')) {
    Sys.sleep(2) # Arbitrary
    batch_response <- httr::GET(paste0('https://api.tomtom.com', location))
    status <- httr::status_code(batch_response)
    if (verbose) httr::message_for_status(batch_response)
  }
  
  if (status == '200') {
    if (verbose) message("Batch downloaded")
    raw_content <- httr::content(batch_response, as = 'text', encoding = 'UTF-8')
  } else {
    stop(paste0('Batch failed. Status code: ', status), call. = TRUE)
  }
  
} else {
  raw_content <- httr::content(response, as = 'text', encoding = 'UTF-8')
}


# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(raw_content)

# if there were problems with the results then return NA
if (all(content$batchItems$statusCode != 200)){
  # Loop through errors
  for (j in seq_len(length(content$batchItems$statusCode))){
    error_code <- content$batchItems$statusCode[1]
    if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(error_code)))
    if ('errorText' %in% names(content$batchItems$response)) {
      message(paste0('Error: ', content$batchItems$response$errorText[j]))
    }
  }
  return(NA_value)
}



# result_list is a list of dataframes
result_list <- content$batchItems$response$results

# if no results are returned for a given address then there is a 0 row dataframe in this
# list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
result_list_filled <- lapply(result_list, filler_df, c('lat', 'long'))

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
results <- dplyr::bind_rows(result_list_filled)

# rename lat/long columns
results$lat <- results$position$lat
results$long <- results$position$lon

# extract address column dataframe
tomtom_address <- results$address
names(tomtom_address) <- paste0('tomtom_address.', names(tomtom_address))
results <- results[!names(results) %in% c('address')]
results <- tibble::as_tibble(cbind(results, tomtom_address))

names(results)[names(results) == 'lat'] <- lat
names(results)[names(results) == 'long'] <- long

if (full_results == FALSE) return(results[c(lat, long)])
else return(cbind(results[c(lat, long)], results[!names(results) %in% c(lat, long)]))

# Live test -----
library(tibble)
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  lat = "latt",
  long = "longgg",
  verbose = TRUE
)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  limit = 5,
  full_results = TRUE,
  return_addresses = FALSE,
  verbose = TRUE
)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  limit = 5,
  full_results = TRUE,
  return_addresses = FALSE,
  verbose = FALSE
)

# Force errors
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  lat = "latt",
  long = "longgg",
  verbose = TRUE,
  custom_query = list(key="aa")
)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  lat = "latt",
  long = "longgg",
  verbose = FALSE,
  custom_query = list(key="aa")
)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  lat = "latt",
  long = "longgg",
  verbose = TRUE,
  custom_query = list(language =" aa")
)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "tomtom",
  lat = "latt",
  long = "longgg",
  verbose = FALSE,
  custom_query = list(language =" aa")
)

# End errors

ss <- tidygeocoder::geo(
  address = c("Plaza Mayor", "xxxxxxxxx", "George Street", "-----"),
  method = "tomtom",
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  verbose = TRUE,
  custom_query = list(language = "de-DE")
)
glimpse(ss)

# Final test - huge batch - not run, daily limit is 2500 ----
library(mapSpain)
library(tibble)
library(dplyr)

address <- tibble(direcciones = mapSpain::esp_munic.sf$name) %>%
  slice(1:40)

address_geo <- address %>%
  geocode(
    address = "direcciones", method = "tomtom", full_results = TRUE,
    verbose = TRUE,
    custom_query = list(waitTimeSeconds = 5)
  )


address_geo

address %>%
  geocode(
    address = "direcciones", method = "tomtom", full_results = TRUE,
    verbose = FALSE,
    custom_query = list(waitTimeSeconds = 5)
  )

