### Functions for reverse batch geocoding that are called by reverse_geo()


# Reverse Batch geocoding with geocodio
# ... are arguments passed from the reverse_geo() function
# https://www.geocod.io/docs/#batch-geocoding
reverse_batch_geocodio <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                   verbose = FALSE, api_url = NULL, api_options = list(), limit = 1, ...) {
  
  if (is.null(api_url)) api_url <- get_geocodio_url(api_options[["geocodio_v"]], 
                                      reverse = TRUE, geocodio_hipaa = api_options[["geocodio_hipaa"]])
  
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = limit, api_key = get_key('geocodio')),
                                    custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  response <- query_api(api_url, query_parameters, mode = 'list', 
                           input_list = paste0(as.character(lat), ',', as.character(long)), 
                           timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  response_parsed <- jsonlite::fromJSON(response$content, flatten = TRUE)
  result_list <- response_parsed$results$response.results
  
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
                               verbose = FALSE, api_url = NULL, limit = 1,
                               api_options = list(), ...) {
  
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
  
  if (!is.null(api_options[["here_request_id"]])){
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
    api_options[["here_request_id"]] <- job_result$Response$MetaInfo$RequestId
  }
  
  if (verbose) message('HERE: RequestID -> ', api_options[["here_request_id"]])
  
  # Step 2: Check job until is done ----
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/job-status.html
  current_status <- ''
  
  if (verbose) message('\nHERE: Batch job:')
  
  # HERE Batching takes a while!
  while (!current_status %in% c('cancelled', 'failed', 'completed')) {
    Sys.sleep(3) # Arbitrary, 3sec
    status <- httr::GET(url = paste0(api_url, '/', api_options[["here_request_id"]]),
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
    delete <- httr::DELETE(url = paste0(api_url, '/', api_options[["here_request_id"]]),
                           query = list(apiKey = get_key('here')))
    
    if (verbose) message('\nHERE: Batch job failure\n')
    return(NA_batch)
  }
  
  # Step 3: GET results and parse ----
  batch_results <-
    httr::GET(url = paste0(api_url, '/', api_options[["here_request_id"]], '/result'),
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
# ... are arguments passed from the reverse_geo() function
# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/asynchronous-batch-submission
reverse_batch_tomtom <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                 verbose = FALSE, api_url = NULL, limit = 1, ...) {
  
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
      extract_errors_from_results('tomtom', raw_content, verbose)
      return(NA_value)        }
    
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
      if ('error' %in% names(content$batchItems$response)) {
        message(paste0('Error: ', content$batchItems$response$error[j]))
      }
    }
    return(NA_value)
  }
  
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


# Reverse Batch geocoding with mapquest
# ... are arguments passed from the reverse_geo() function
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
reverse_batch_mapquest <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                   verbose = FALSE, api_url = NULL, api_options = list(), limit = 1, ...) {


  latLng <- as.character(paste0(lat, ',', long))
  
  NA_value <- get_na_value(address, "xxx", rows = length(latLng))[address] # filler result to return if needed
  
  # Construct query
  # Depends if single or multiple query
  
  # Single: Now allowed on batch, return a single query ----
  if (length(latLng) == 1) {
    results <- reverse_geo(lat = lat, long = long, mode = 'single', method = 'mapquest',
                           full_results = full_results, custom_query = custom_query, 
                           verbose = verbose, api_url = api_url, limit = limit, 
                           mapquest_open = api_options[["mapquest_open"]])
    
    # rename lat/long columns
    names(results)[names(results) == 'address'] <- address
    
    return(results[!names(results) %in% c('lat', 'long')])
  }
  
  # Multiple via POST ----
  # https://developer.mapquest.com/documentation/geocoding-api/batch/post/
  if (is.null(api_url)) {
    url_domain <- if (api_options[["mapquest_open"]]) "http://open" else  "http://www"
    
    api_url <- paste0(url_domain, ".mapquestapi.com/geocoding/v1/batch")
  }
  
  # Construct query - for display only
  
  query_parameters <- get_api_query("mapquest", 
                                    list(limit = limit, api_key = get_key("mapquest")),
                                    custom_parameters = custom_query)
  
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # https://developer.mapquest.com/documentation/geocoding-api/batch/post/
  # Construct POST query
  
  # A. Only certain parameters should be in the POST call----
  
  body_params <- query_parameters[!names(query_parameters) %in% c("key", "callback")]
  query_parameters <- query_parameters[names(query_parameters) %in% c("key", "callback")]
  
  # B. Construct Body----
  coords_list <- list(
    locations = latLng,
    options = body_params
  )
  
  ## Query API ----
  
  query_results <- query_api(api_url, query_parameters, mode = "list",
                             input_list = coords_list, timeout = timeout)
  
  # C. Error handling----
  # Parse result code
  if (jsonlite::validate(query_results$content)) {
    status_code <- jsonlite::fromJSON(query_results$content, flatten = TRUE)$info$statuscode
  } else {
    status_code <- query_results$status
  }
  # Successful status_code is 0
  if (status_code == "0") status_code <- "200"
  status_code <- as.character(status_code)
  
  if (verbose == TRUE) message(paste0("HTTP Status Code: ", as.character(status_code)))
  
  ## Extract results -----------------------------------------------------------------------------------
  # if there were problems with the results then return NA
  if (status_code != "200") {
    if (!jsonlite::validate(query_results$content)) {
      # in cases like this, display the raw content but limit the length
      # in case it is really long.
      message(paste0("Error: ", strtrim(as.character(query_results$content), 100)))
    } else {
      # Parse and get message
      content <- jsonlite::fromJSON(query_results$content, flatten = TRUE)
      if (!is.null(content$info$messages)) message(paste0("Error: ", content$info$messages))
    }
    # Return empty and exit
    return(NA_value)
  }
  # D. On valid API response-----
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(query_results$content, flatten = TRUE)
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  result_list <- content$results$locations
  result_list_filled <- lapply(result_list, filler_df, c('street'))
  results <- dplyr::bind_rows(result_list_filled)
  
  # Format address
  frmt_address <- format_address(results, c('street', paste0('adminArea', seq(6, 1))))
  results <- tibble::as_tibble(cbind(frmt_address, results))
  
  names(results)[names(results) == 'formatted_address'] <- address
  
  if (full_results == FALSE)  return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))  
}

# Reverse Batch geocoding with Bing
# ... are arguments passed from the reverse_geo() function
# https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/
reverse_batch_bing <- function(lat, long, address = 'address', timeout = 20, full_results = FALSE, custom_query = list(),
                                   verbose = FALSE, api_url = NULL, ...) {
  # Specific endpoint
  if (is.null(api_url)) api_url <- 'http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode'
  
  
  latlon_df <- tibble::tibble(Id = seq_len(length(lat)),
                              Latitude = lat,
                              Longitude = long)
  names(latlon_df) <- c("Id", "ReverseGeocodeRequest/Location/Latitude", 
                        "ReverseGeocodeRequest/Location/Longitude")
  
  # filler result to return if needed
  NA_batch <- get_na_value(address, "xxx", rows = nrow(latlon_df))[address]
  
  # Construct query ----
  # Bing needs a special list of params
  # https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/
  query_parameters <- get_api_query('bing',
                                    list(api_key = get_key('bing')),
                                    custom_parameters = c(list(input = 'pipe'), custom_query)
  )
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Create body of the POST request----
  # Needs to have Id and some fields, already on latlon_df
  # Also needs to add response fields
  response_fields <- c('GeocodeResponse/Address/AddressLine',
                       'GeocodeResponse/Address/AdminDistrict',
                       'GeocodeResponse/Address/CountryRegion',
                       'GeocodeResponse/Address/AdminDistrict2',
                       'GeocodeResponse/Address/FormattedAddress',  
                       'GeocodeResponse/Address/Locality',
                       'GeocodeResponse/Address/PostalCode',
                       'GeocodeResponse/Address/PostalTown',
                       'GeocodeResponse/Address/Neighborhood',
                       'GeocodeResponse/Address/Landmark',
                       'GeocodeResponse/Confidence',
                       'GeocodeResponse/Name',
                       'GeocodeResponse/EntityType',
                       'GeocodeResponse/MatchCodes',
                       'GeocodeResponse/Point/Latitude',
                       'GeocodeResponse/Point/Longitude',
                       'GeocodeResponse/BoundingBox/SouthLatitude',
                       'GeocodeResponse/BoundingBox/WestLongitude',
                       'GeocodeResponse/BoundingBox/NorthLatitude',
                       'GeocodeResponse/BoundingBox/EastLongitude')  
  
  # Create mock cols
  mock <- as.data.frame(
    matrix(data="", ncol=length(response_fields), nrow = nrow(latlon_df))
  )
  names(mock) <- response_fields
  
  # Create tibble for body
  latlon_body <- dplyr::bind_cols(latlon_df, mock)
  
  # Create file
  body_file <- tempfile()
  cat("Bing Spatial Data Services, 2.0", file=body_file, append=FALSE, sep = "\n")
  cat(paste0(names(latlon_body), collapse = "|"), file=body_file, append=TRUE, sep = "\n")
  for (j in (seq_len(nrow(latlon_body)))){
    body <- paste0(latlon_body[j, ], collapse = "|")
    body <- gsub('NA','',body)
    cat(body, file=body_file, append=TRUE, sep = "\n")
  }
  # Body created on body_file
  
  # Step 1: Run job and retrieve id ----
  # Modification from query_api function
  if (verbose) message('\nBing: Batch job:')
  
  # Batch timer
  init_process <- Sys.time()
  job <- httr::POST(api_url,
                    query = query_parameters,
                    body = httr::upload_file(body_file),
                    httr::timeout(60 * timeout)
  )
  httr::warn_for_status(job)
  status_code <- httr::status_code(job)
  job_result <- httr::content(job)
  
  # On error return NA
  if (status_code != '201'){
    if (verbose) message(paste0("Error: ", job_result$errorDetails))  
    return(NA_batch)
  }
  
  jobID <- job_result$resourceSets[[1]]$resources[[1]]$id
  
  # Step 2: Check job until is done ----
  if (verbose) {
    httr::message_for_status(job)
    # Force new line
    message()
  }
  
  current_status <- ''
  
  while (current_status %in%  c('Pending', '')) {
    Sys.sleep(3) # Arbitrary, 3sec
    status <- httr::GET(url = paste0(api_url, '/', jobID),
                        query = list(key = get_key('bing'))
    )
    status_get <- httr::content(status)
    
    prev_status <- current_status
    current_status <- status_get$resourceSets[[1]]$resources[[1]]$status
    
    if (verbose && prev_status != current_status){
      message(paste0("Bing: ",current_status))
    }
  }
  
  status_results <- status_get$resourceSets[[1]]$resources[[1]]
  process <- as.integer(status_results$processedEntityCount)
  errors <- as.integer(status_results$failedEntityCount)
  succees <- process - errors
  
  if (verbose) {
    httr::message_for_status(job)
    # Force new line
    message()
    message(paste0('Bing: Processed: ', process,
                   " | Succeeded: ", succees,
                   " | Failed: ", errors))
  }
  
  update_time_elapsed <- get_seconds_elapsed(init_process)
  
  if (verbose) print_time('Bing: Batch job processed in', update_time_elapsed)
  
  
  # Step 3: GET results and parse ----
  links <- status_get$resourceSets[[1]]$resources[[1]]$links
  
  # If not succeeded return NA
  if (process == errors){
    if (verbose) message("Bing: All failed")
    return(NA_batch)
  }
  
  # Download and parse succeeded results
  
  batch_results <-
    httr::GET(url = links[[2]]$url,
              query = list(key = get_key('bing'))
    )
  
  result_content <- httr::content(batch_results, as = 'text', encoding = 'UTF-8')
  
  # Skip first line
  result_parsed <- tibble::as_tibble(utils::read.table(text = result_content,
                                                       skip = 1,
                                                       header = TRUE, 
                                                       sep = "|"))
  # Merge to original latlons and output----
  base <- tibble::as_tibble(latlon_df)
  results <- merge(base["Id"], 
                   result_parsed, 
                   all.x = TRUE)
  
  names(results)[names(results) == 'GeocodeResponse.Address.FormattedAddress'] <- address
  
  if (full_results == FALSE) return(results[address])
  else return(cbind(results[address], results[!names(results) %in% c(address)]))
  
}