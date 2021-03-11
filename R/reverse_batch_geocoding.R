### Functions for reverse batch geocoding that are called by reverse_geo()


# Reverse Batch geocoding with geocodio
# ... are arguments passed from the reverse_geo() function
# https://www.geocod.io/docs/#batch-geocoding
reverse_batch_geocodio <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
verbose = FALSE, api_url = NULL, geocodio_v = 1.6, limit = 1, ...) {
  
  if (is.null(api_url)) api_url <- get_geocodio_url(geocodio_v, reverse = TRUE)
  
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = limit, api_key = get_key('geocodio')),
                                    custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'list', 
                  input_list = paste0(as.character(lat), ',', as.character(long)), 
                  timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  response <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  result_list <- response$results$response.results
  
  # if no results are returned for a given coordinate then there is a 0 row dataframe in this
  # list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
  result_list_filled <- lapply(result_list, filler_df, c('formatted_address'))
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  results <- dplyr::bind_rows(result_list_filled)
  
  names(results)[names(results) == 'formatted_address'] <- address
  
  if (full_results == FALSE)  return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))
}

# Reverse Batch geocoding with tomtom
# ... are arguments passed from the geo() function
# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/asynchronous-batch-submission
reverse_batch_tomtom <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                   verbose = FALSE, api_url = NULL, limit = 1, ...) {

  if (is.null(api_url)) api_url <- 'https://api.tomtom.com/search/2/batch.json'
  
  # Construct query
  query_parameters <- get_api_query('tomtom',
                                    list(limit = limit, api_key = get_key('tomtom')),
                                    custom_parameters = custom_query)
  
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Parameters needs to be included on each element
  q_elements <- query_parameters[names(query_parameters) != 'key']
  
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
  raw_content <- query_api(api_url, query_parameters, mode = 'list',
                           input_list = address_list, timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content)
  
  # result_list is a list of dataframes
  result_list <- content$batchItems$response$addresses
  
  # if no results are returned for a given coordinate then there is a 0 row dataframe in this
  # list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
  result_list_filled <- lapply(result_list, filler_df, c('position'))
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  results <- dplyr::bind_rows(result_list_filled)
  
  #Unpack addresses
  tomtom_address <- results$address
  results <- results[!names(results) %in% c('address')]
  results <- tibble::as_tibble(cbind(results, tomtom_address))
  
  names(results)[names(results) == 'freeformAddress'] <- address
  
  if (full_results == FALSE) return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))
  
}