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

# Reverse batch geocoding with HERE
# ... are arguments passed from the reverse_geo() function
# https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/introduction.html
reverse_batch_here <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                               verbose = FALSE, api_url = NULL, geocodio_v = 1.6, limit = 1,
                               here_request_id = NULL, ...) {
  
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/quick-start-batch-geocode.html
  # Specific endpoint
  if (is.null(api_url)) api_url <- 'https://batch.geocoder.ls.hereapi.com/6.2/jobs'
  
  # Create tibble to be passed to the body
  # Radius of the search (m), required param on reverse
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/data-input.html
  radius <- '250' # Discretional, as per the example on API docs
  
  latlon_df <- tibble::tibble(prox = paste0(as.character(lat), ',', as.character(long), ',', radius))
  latlon_df <- tibble::add_column(latlon_df, recId = seq_len(nrow(latlon_df)), .before = 'prox')  
  
  # filler result to return if needed
  NA_batch <- tibble::tibble(address = rep(as.character(NA), nrow(latlon_df))) # filler NA result to return if needed
  names(NA_batch)[1] <- address # rename column
  
  # Construct query ----
  # HERE needs a special list of params - create with no override
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/request-parameters.html
  
  # Output structure differs from single geocoding
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/read-batch-request-output.html
  # These output cols has been selected under own criteria - can be modified
  
  # Minimum parameters: locationLabel
  
  if (full_results) {
    outcols <- c('locationLabel,displayLatitude,displayLongitude',
                 'street', 'district', 'city', 'postalCode',
                 'county', 'state', 'country', 'relevance', 
                 'mapViewBottomRightLatitude', 'mapViewBottomRightLongitude',
                 'mapViewTopLeftLatitude', 'mapViewTopLeftLongitude'
    )
  } else {
    # Minimum params
    outcols <- c('locationLabel')
  }
  
  custom_here_query <- list(maxresults = limit,
                            indelim = '|',
                            outdelim = '|', # Required
                            outputcombined = TRUE,  # Required
                            mode = 'retrieveAddresses', #Required
                            outcols = paste0(outcols, collapse = ','),
                            includeInputFields = TRUE
  )
  
  # Clean parameters of default HERE query and combine
  custom_here_query <- custom_here_query[!names(custom_here_query) %in% names(custom_query)]
  
  # Manage minimum pars if passed via custom_query
  if ('outcols' %in% names(custom_query)) {
    custom_query['outcols'] <- paste0('locationLabel,', custom_query['outcols'][[1]])
  }
  
  # Merge custom and HERE query
  custom_query <- c(custom_query, custom_here_query)
  query_parameters <- get_api_query('here',
                                    list(limit = limit, api_key = get_key('here')),
                                    custom_parameters = custom_query)
  
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Create body of the POST request----
  # Needs to have recID and prox  
  
  # Plain text, \n new line using indelim
  body <- paste(paste0('recID', query_parameters[['indelim']],'prox\n'),
                paste(latlon_df$recId, query_parameters[['indelim']],
                      latlon_df$prox, collapse = '\n')
  )
  
  # HERE Batch Geocoder is a 3 step process:
  # 1. Send the request and get a job id
  # 2. Wait - Status of the job can be checked
  # 3. Results
  # Exception if a previous job is requested go to Step 2
  
  # Batch timer
  init_process <- Sys.time()
  
  if (!is.null(here_request_id)){
    if (verbose) message("HERE: Requesting a previous job")
    
  } else {
    
    # Step 1: Run job and retrieve id ----
    # Modification from query_api function
    job <- httr::POST(api_url,
                      query = c(query_parameters, action = 'run'),
                      body = body,
                      encode = 'raw',
                      httr::timeout(60 * timeout)
    )
    
    job_result <- httr::content(job)
    
    # On error
    if (is.null(job_result$Response$MetaInfo$RequestId)) {
      message(paste0('Error: ', job_result$Details))
      return(NA_batch)
    }
    
    # Retrieve here_request_id
    here_request_id <- job_result$Response$MetaInfo$RequestId
  }
  
  if (verbose) message('HERE: RequestID -> ', here_request_id)
  
  # Step 2: Check job until is done ----
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/job-status.html
  current_status <- ''
  
  if (verbose) message('\nHERE: Batch job:')
  
  # HERE Batching takes a while!
  while (!current_status %in% c('cancelled', 'failed', 'completed')) {
    Sys.sleep(3) # Arbitrary, 3sec
    status <- httr::GET(url = paste0(api_url, '/', here_request_id),
                        query = list(action = 'status',
                                     apiKey = get_key('here'))
    )
    
    status_get <- httr::content(status)
    prev_status <- current_status
    current_status <- as.character(status_get$Response$Status)
    
    if (verbose) {
      if (prev_status != current_status) message('Status: ', current_status)
      if (current_status == 'running') {
        message('Total ', status_get$Response$TotalCount, ' | ',
                'Processed: ', status_get$Response$ProcessedCount, ' | ',
                'Pending: ', status_get$Response$PendingCount, ' | ',
                'Errors: ', status_get$Response$ErrorCount
        )
      }
    }
  }
  
  update_time_elapsed <- get_seconds_elapsed(init_process)
  
  if (verbose) print_time('HERE: Batch job processed in', update_time_elapsed)
  
  # Delete non-completed jobs and return empty
  if (current_status != 'completed') {
    delete <- httr::DELETE(url = paste0(api_url, '/', here_request_id),
                           query = list(apiKey = get_key('here')))
    
    if (verbose) message('\nHERE: Batch job failure\n')
    return(NA_batch)
  }
  
  # Step 3: GET results and parse ----
  batch_results <-
    httr::GET(url = paste0(api_url, '/', here_request_id, '/result'),
              query = list(apiKey = get_key('here'),
                           outputcompressed = FALSE)
    )
  
  result_content <- httr::content(batch_results)
  
  # Parse results----
  # dlm was requested on custom_here_query - 
  result_parsed <- tibble::as_tibble(utils::read.table(text = result_content,
                                                       header = TRUE, 
                                                       sep = query_parameters[['outdelim']]
  )
  )
  
  # Merge to original addresses and output
  results <- merge(latlon_df[ ,'recId'], 
                   result_parsed, 
                   by = 'recId', 
                   all.x = TRUE)
  
  names(results)[names(results) == 'locationLabel'] <- address
  
  if (full_results == FALSE)  return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))
}


# Reverse Batch geocoding with tomtom
# ... are arguments passed from the geo() function
# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/asynchronous-batch-submission
reverse_batch_tomtom <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                 verbose = FALSE, api_url = NULL, limit = 1, ...) {
  
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
  
  for (index in seq_len(length(lat))) {
    address_list$batchItems[[index]] <-
      list(query = paste0('/reverseGeocode/', lat[index], ',',
                          long[index], '.json', q_string))
  }
  
  # Query API
  response <- httr::POST(api_url, query = api_query_params, 
                         body = as.list(address_list), 
                         encode = 'json', httr::timeout(60 * timeout))
  
  httr::warn_for_status(response)
  
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
