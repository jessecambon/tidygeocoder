api_url <- NULL
timeout <- 20
limit <- 1

lat <- runif(50, 38, 42)
long <- runif(50, -6, 1)

# white house, toronto, junk lat/lng
# lat <- c(38.89586, 300, 43.6534817)
# long <- c(-77.0307713, 500, -79.3839347)
address <- "addr"
verbose <- TRUE
custom_query <- list(
  language = "aaaaaa",
  waitTimeSeconds = 5
)

if (is.null(api_url)) api_url <- 'https://api.tomtom.com/search/2/batch.json'


NA_value <- get_na_value(address,"xxx", length(lat))[address] # filler result to return if needed

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

for (index in seq_len(length(lat))) {
  address_list$batchItems[[index]] <-
    list(query = paste0('/reverseGeocode/', lat[index], ',',
                        long[index], '.json', q_string))
}

# Query API
response <- httr::POST(api_url, query = api_query_params, 
                       body = as.list(address_list), 
                       encode = 'json', httr::timeout(60 * timeout))

if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(httr::status_code(response))))

## Extract results -----------------------------------------------------------------------------------
# if there were problems with the results then return NA
if (!httr::status_code(response) %in% c(200, 202, 303)) {
  content <- httr::content(response, as = 'text', encoding = 'UTF-8')
  extract_errors_from_results('tomtom', content, verbose)
  return(NA_value)
}

# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/asynchronous-batch-submission#response-data
# if status code is not 200 we have to perform a GET and download the batch asynchronously
# On 200 the batch is provided in the response object
if (httr::status_code(response) != '200') {
  if (verbose) message('Asynchronous Batch Download')
  
  # A HTTP Response with a Location header that points where the batch results can be obtained.
  location <- httr::headers(response)$location
  
  status <- httr::status_code(response)
  while (status %in% c('202', '303')) {
    Sys.sleep(2) # Arbitrary
    batch_response <- httr::GET(paste0('https://api.tomtom.com', location))
    status <- httr::status_code(batch_response)
    if (verbose) httr::message_for_status(batch_response)
  }
  
  if (verbose == TRUE) message(paste0('\nHTTP Status Code: ', status))
  
  if (status == '200') {
    if (verbose) message('Batch downloaded')
    raw_content <- httr::content(batch_response, as = 'text', encoding = 'UTF-8')
  } else {
    # if there were problems with the results then return NA
    raw_content <- httr::content(batch_response, as = 'text', encoding = 'UTF-8')
    extract_errors_from_results('tomtom', query_results$content, verbose)
    return(NA_value)        }
  
} else {
  raw_content <- httr::content(response, as = 'text', encoding = 'UTF-8')
}

j = 1
seq_len(length(content$batchItems$statusCode))
# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(raw_content)
all(content$batchItems$statusCode != 200)
# if there were problems with the results then return NA
if (all(content$batchItems$statusCode != 200)){
  # Loop through errors
  for (j in seq_len(length(content$batchItems$statusCode))){
    error_code <- content$batchItems$statusCode[j]
    if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(error_code)))
    if ('errorText' %in% names(content$batchItems$response)) {
      message(paste0('Error: ', content$batchItems$response$errorText[j]))
    }
  }
  return(NA_value)
}
a <- content$batchItems$response
# result_list is a list of dataframes
result_list <- content$batchItems$response$addresses

# if no results are returned for a given coordinate then there is a 0 row dataframe in this
# list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
result_list_filled <- lapply(result_list, filler_df, c("position"))

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
results <- dplyr::bind_rows(result_list_filled)

# Unpack addresses
tomtom_address <- results$address
results <- results[!names(results) %in% c("address")]
results <- tibble::as_tibble(cbind(results, tomtom_address))

names(results)[names(results) == "freeformAddress"] <- address

# if (full_results == FALSE)  return(results[address])
# else return(cbind(results[address], results[!names(results) %in% c(address)]))

# Live tests----

# test errors
tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "tomtom",
  verbose = TRUE,
  address = "aaaadddd",
  limit = 1,
  custom_query = list(key="ss")
)

tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "tomtom",
  verbose = TRUE,
  address = "ssss",
  limit = 1,
  custom_query = list(language="sssss")
)


#end

tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "tomtom",
  verbose = TRUE,
  limit = 1
)




tidygeocoder::reverse_geo(
  lat = c(38.89586, 300, 43.6534817),
  long = c(-77.0307713, 500, -79.3839347),
  method = "tomtom",
  address = "aaa",
  verbose = TRUE,
  mode = "batch",
  return_coords = FALSE,
  full_results = TRUE,
  limit = 1
)

# Single on batch
tidygeocoder::reverse_geo(
  lat = c(38.89586),
  long = c(-77.0307713),
  method = "tomtom",
  mode = "batch",
  address = "abcde",
  verbose = TRUE,
  full_results = TRUE,
  limit = 1
)

tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "tomtom",
  address = "direccion",
  verbose = TRUE,
  full_results = TRUE,
  return_coords = FALSE,
  limit = 4
)




test3 <- tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "tomtom",
  verbose = TRUE,
  full_results = TRUE,
  limit = 1,
  custom_query = list(
    language = "de-DE"
  )
)

tibble::glimpse(test3)



test5 <- tidygeocoder::reverse_geo(
  lat = c(48.8582, 40.45305),
  long = c(2.2944, -3.68834),
  method = "tomtom",
  verbose = FALSE,
  full_results = TRUE,
  return_coords = FALSE,
  limit = 30,
  custom_query = list(
    radius = 20000
  )
)
test5

tidygeocoder::reverse_geo(
  lat = c(48.8582, 40.45305),
  long = c(2.2944, -3.68834),
  method = "tomtom",
  no_query = TRUE
)

# Bulk query - not test ----


bulk <- tidygeocoder::reverse_geo(
  lat = runif(50, 38, 42),
  long = runif(50, -6, 1),
  method = "tomtom",
  verbose = TRUE,
  custom_query = list(
    language = "fr-FR",
    waitTimeSeconds = 5
  ) # This is the thresold to determine if async should be done
)

tidygeocoder::reverse_geo(
  lat = runif(50, 38, 42),
  long = runif(50, -6, 1),
  method = "tomtom",
  verbose = TRUE,
  custom_query = list(
    language = "aaaaa",
    waitTimeSeconds = 5
  ) # This is the thresold to determine if async should be done
)
