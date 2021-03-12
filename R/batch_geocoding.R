### Functions for batch geocoding that are called by geo()

# Census batch geocoding
# @param address_pack packaged addresses object
# Vintage must be defined if return = 'geographies'
batch_census <- function(unique_addresses,
     return_type = 'locations', timeout = 20, full_results = FALSE, custom_query = list(), api_url = NULL,
     lat = 'lat', long = 'long', verbose = FALSE, ...) {
  
  if (!'street' %in% names(unique_addresses) & (!'address' %in% names(unique_addresses))) {
    stop("To use the census geocoder, either 'street' or 'address' must be defined")
  }
  
  location_cols <- c('id', 'input_address', 'match_indicator', 'match_type','matched_address', 
          'coords', 'tiger_line_id', 'tiger_side')
  return_cols <- switch(return_type,
          'locations' = location_cols,
          'geographies' = c(location_cols, c('state_fips', 'county_fips', 'census_tract', 'census_block'))
  )
  
  if (is.null(api_url)) api_url <- get_census_url(return_type, 'addressbatch')
  
  num_addresses <- nrow(unique_addresses)

  # create input dataframe
  input_df <- tibble::tibble(
    id      = 1:num_addresses,
    street  = if ('street' %in% names(unique_addresses)) unique_addresses$street else unique_addresses$address,
    city    = if ('city' %in% names(unique_addresses)) unique_addresses$city else NA,
    state   = if ('state' %in% names(unique_addresses)) unique_addresses$state else NA,
    zip     = if ('postalcode' %in% names(unique_addresses)) unique_addresses$postalcode else NA
  )
  
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(input_df, tmp, row.names = FALSE, col.names = FALSE, sep = ',', na = '')
  
  # Construct query
  # NOTE - request will fail if vintage and benchmark are invalid for return_type = 'geographies'
  query_parameters <- get_api_query('census', custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'file', 
          batch_file = tmp, content_encoding = "ISO-8859-1", timeout = timeout)
  
  # force certain geographies columns to be read in as character instead of numeric
  # to preserve leading zeros (for FIPS codes)
  column_classes <- ifelse(return_type == 'geographies',
       c('state_fips' = 'character',
         'county_fips' = 'character',
         'census_tract' = 'character',
         'census_block' = 'character'),
          NA)
  
  results <- utils::read.csv(text = raw_content, header = FALSE,
       col.names = return_cols,
       colClasses = column_classes,
       fill = TRUE, stringsAsFactors = FALSE,
       na.strings = '')
  
  # convert 'id' to integer since we sort on it
  results[['id']] <- as.integer(results[['id']])
  
  # make sure results remain in proper order
  results <- results[order(results[['id']]), ]

  # split out lat/lng. lapply is used with as.numeric to convert coordinates to numeric
  coord_df <- do.call(rbind, lapply(results$coords, split_coords))
  colnames(coord_df) <- c(long, lat)  # <--- NOTE ORDER
  
  # convert to tibble and reorder coordinates
  coord_df <- tibble::as_tibble(coord_df)[c(lat, long)]

  if (full_results == FALSE) return(coord_df)
  else {
    # Combine extracted lat/longs with other return results
    combi <- tibble::as_tibble(dplyr::bind_cols(coord_df, results[!names(results) %in% c('coords')]))
    return(combi)
  }
}


# Batch geocoding with geocodio
# ... are arguments passed from the geo() function
# https://www.geocod.io/docs/#batch-geocoding
batch_geocodio <- function(unique_addresses, lat = 'lat', long = 'long', timeout = 20, full_results = FALSE, custom_query = list(),
verbose = FALSE, api_url = NULL, geocodio_v = 1.6, limit = 1, ...) {
  
  # limit the dataframe to legitimate arguments
  address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters('geocodio', address_only = TRUE)]
  
  ## If single line addresses are passed then we will package them as a single list
  if ('address' %in% names(address_df)) {
    address_list <- as.list(address_df[['address']])
  } else {
    # if address components are passed then ...
    # convert dataframe into named lists which we will pass to the geocoder via httr::POST
    address_list <- list()
    for (index in 1:nrow(address_df)) {
      address_list[[index]] <- as.list(address_df[index,])
    }
    names(address_list) <- 1:nrow(address_df)
  }
  
  if (is.null(api_url)) api_url <- get_geocodio_url(geocodio_v)
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = limit, api_key = get_key('geocodio')),
                                    custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'list', input_list = address_list, timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  
  # How results are parsed depends on whether single line addresses or address
  # components were passed
  # result_list is a list of dataframes
  if ('address' %in% names(address_df)) {
    result_list <- content$results$response.results
  } else {
    result_list <- lapply(content$results, function(x) x$response$results)
  }
  
  # if no results are returned for a given address then there is a 0 row dataframe in this
  # list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
  result_list_filled <- lapply(result_list, filler_df, c('location.lat','location.lng'))
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  results <- dplyr::bind_rows(result_list_filled)
  
  # rename lat/long columns
  names(results)[names(results) == 'location.lat'] <- lat
  names(results)[names(results) == 'location.lng'] <- long
  
  if (full_results == FALSE)  return(results[c(lat, long)])
  else return(cbind(results[c(lat,long)], results[!names(results) %in% c(lat, long)]))
}


# Batch geocoding with HERE
# ... are arguments passed from the geo() function
# https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/introduction.html
batch_here <- function(unique_addresses, lat = 'lat', long = 'long', timeout = 20, full_results = FALSE, custom_query = list(),
                       verbose = FALSE, api_url = NULL, geocodio_v = 1.6, limit = 1, 
                       here_request_id = NULL, ...) {

  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/quick-start-batch-geocode.html
  # Specific endpoint
  if (is.null(api_url)) api_url <- 'https://batch.geocoder.ls.hereapi.com/6.2/jobs'
  
  address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters('here', address_only = TRUE)]

  # filler result to return if needed
  NA_batch <- get_na_value(lat, long, rows = nrow(address_df))
  
  # Construct query ----
  # HERE needs a special list of params - create with no override
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/request-parameters.html
  
  # Output structure differs from single geocoding
  # https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/read-batch-request-output.html
  # These output cols has been selected under own criteria - can be modified
  
  # Minimum parameters: displayLatitude,displayLongitude
  
  if (full_results) {
    outcols <- c('displayLatitude,displayLongitude,locationLabel',
                 'street', 'district', 'city', 'postalCode',
                 'county', 'state', 'country', 'relevance', 
                 'mapViewBottomRightLatitude', 'mapViewBottomRightLongitude',
                 'mapViewTopLeftLatitude', 'mapViewTopLeftLongitude'
                 )
  } else {
    # Minimum params
    outcols <- c('displayLatitude,displayLongitude')
  }
  
  custom_here_query <- list(maxresults = limit,
                            indelim = '|',
                            outdelim = '|', # Required
                            outputcombined = TRUE,  # Required
                            outcols = paste0(outcols, collapse = ','),
                            includeInputFields = TRUE
                            )
  
  # Clean parameters of default HERE query and combine
  custom_here_query <- custom_here_query[!names(custom_here_query) %in% names(custom_query)]
  
  # Manage minimum pars if passed via custom_query
  if ('outcols' %in% names(custom_query)) {
    custom_query['outcols'] <- paste0('displayLatitude,displayLongitude,', custom_query['outcols'][[1]])
  }
  
  # Merge custom and HERE query
  custom_query <- c(custom_query, custom_here_query)
  query_parameters <- get_api_query('here',
                                    list(limit = limit, api_key = get_key('here')),
                                    custom_parameters = custom_query)
  
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Create body of the POST request----
  # Needs to have recID and searchText  
  names(address_df) <- 'searchText'
  address_df <- tibble::add_column(address_df, recId = seq_len(nrow(address_df)), .before = 'searchText')
  
  # Plain text, \n new line using indelim
  body <- paste(paste0('recID', query_parameters[['indelim']],'searchText\n'),
                paste(address_df$recId, query_parameters[['indelim']],
                      address_df$searchText, collapse = '\n')
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
  results <- merge(address_df[ ,'recId'], 
                   result_parsed, 
                   by = 'recId', 
                   all.x = TRUE)
  
  names(results)[names(results) == 'displayLatitude'] <- lat
  names(results)[names(results) == 'displayLongitude'] <- long
  
  if (full_results == FALSE) return(results[c(lat, long)])
  else return(cbind(results[c(lat, long)], results[!names(results) %in% c(lat, long)]))
  
}

# Batch geocoding with TomTom
# ... are arguments passed from the geo() function
# https://developer.tomtom.com/search-api/search-api-documentation-batch-search/asynchronous-batch-submission
batch_tomtom <- function(unique_addresses, lat = 'lat', long = 'long', 
                         timeout = 20, full_results = FALSE,
                         custom_query = list(), verbose = FALSE,
                         api_url = NULL, limit = 1, ...) {
  # limit the dataframe to legitimate arguments
  address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters('tomtom', address_only = TRUE)]
  
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
    q_string <- paste0(q_string, dlm, names(q_elements[par]), '=',  q_elements[[par]])
  }
  
  # Construct body
  address_list <- list(batchItems = list())
  
  for (index in 1:nrow(address_df)) {
    address_list$batchItems[[index]] <- list(query = paste0('/geocode/', as.list(address_df[index, ]), '.json', q_string))
  }
  
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'list',
                           input_list = address_list, timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content)
  
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
}
