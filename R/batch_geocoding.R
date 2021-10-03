### Functions for batch geocoding that are called by geo()

# Census batch geocoding
# @param address_pack packaged addresses object
# Vintage must be defined if return = 'geographies'
batch_census <- function(unique_addresses,
     api_options = list(), timeout = 20, full_results = FALSE, custom_query = list(), api_url = NULL,
     lat = 'lat', long = 'long', verbose = FALSE, ...) {
  
  if (!'street' %in% names(unique_addresses) & (!'address' %in% names(unique_addresses))) {
    stop("To use the census geocoder, either 'street' or 'address' must be defined")
  }
  
  location_cols <- c('id', 'input_address', 'match_indicator', 'match_type','matched_address', 
          'coords', 'tiger_line_id', 'tiger_side')
  return_cols <- switch(api_options[["census_return_type"]],
          'locations' = location_cols,
          'geographies' = c(location_cols, c('state_fips', 'county_fips', 'census_tract', 'census_block'))
  )
  
  if (is.null(api_url)) api_url <- get_census_url(api_options[["census_return_type"]], 'addressbatch')
  
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
  # NOTE - request will fail if vintage and benchmark are invalid for census_return_type = 'geographies'
  query_parameters <- get_api_query('census', custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  response <- query_api(api_url, query_parameters, mode = 'file', 
          batch_file = tmp, content_encoding = "ISO-8859-1", timeout = timeout)
  
  # force certain geographies columns to be read in as character instead of numeric
  # to preserve leading zeros (for FIPS codes)
  column_classes <- ifelse(api_options[["census_return_type"]] == 'geographies',
       c('state_fips' = 'character',
         'county_fips' = 'character',
         'census_tract' = 'character',
         'census_block' = 'character'),
          NA)
  
  results <- utils::read.csv(text = response$content, header = FALSE,
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

batch_geocodio <- function(unique_addresses, lat = 'lat', long = 'long', timeout = 20, 
          full_results = FALSE, custom_query = list(), verbose = FALSE, api_url = NULL, 
        api_options = list(), limit = 1, ...) {
  
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
  
  if (is.null(api_url)) api_url <- get_geocodio_url(api_options[["geocodio_v"]], reverse = FALSE, geocodio_hipaa = api_options[["geocodio_hipaa"]])
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = limit, api_key = get_key('geocodio')),
                                    custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  response <- query_api(api_url, query_parameters, mode = 'list', input_list = address_list, timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(response$content, flatten = TRUE)
  
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
                       verbose = FALSE, api_url = NULL, limit = 1, 
                       api_options = list(), ...) {

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
      return(NA_value)    
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
      error_code <- content$batchItems$statusCode[j]
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
}


# Batch geocoding with mapquest
# ... are arguments passed from the geo() function
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
batch_mapquest <-  function(unique_addresses, lat = "lat", long = "long",
                            timeout = 20, full_results = FALSE, custom_query = list(),
                            verbose = FALSE, api_url = NULL, limit = 1, 
                            api_options = list(), ...) {
    # limit the dataframe to legitimate arguments
    address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters("mapquest", address_only = TRUE)]

    NA_value <- get_na_value(lat, long, rows = nrow(address_df)) # filler result to return if needed

    # Construct query
    # Depends if single or multiple query

    # Single: Now allowed on batch, return a single query ----
    if (nrow(address_df) == 1) {
      results <- geo(address = address_df[["address"]], method = "mapquest",
                     mode = "single", full_results = full_results,
                     custom_query = custom_query, verbose = verbose,
                     api_url = api_url, limit = limit,
                     mapquest_open = api_options[["mapquest_open"]])

      # rename lat/long columns
      names(results)[names(results) == "lat"] <- lat
      names(results)[names(results) == "long"] <- long

      return(results[!names(results) %in% "address"])
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
    address_list <- list(
      locations = address_df[["address"]],
      options = body_params
    )

    ## Query API ----
    query_results <- query_api(api_url, query_parameters, mode = "list",
                               input_list = address_list, timeout = timeout)

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
    # MapQuest always return a default value (lat:39.4 long:-99.1) for non-found addresses
    results <- dplyr::bind_rows(content$results$locations)

    # rename lat/long columns
    names(results)[names(results) == "latLng.lat"] <- lat
    names(results)[names(results) == "latLng.lng"] <- long
    
    # Format address
    frmt_address <- format_address(results, c('street', paste0('adminArea', seq(6, 1))))
    results <- tibble::as_tibble(cbind(frmt_address, results))

    ## Prepare output----
    if (full_results == FALSE) return(results[c(lat, long)])
    else return(cbind(results[c(lat, long)], results[!names(results) %in% c(lat, long)]))
}


# Batch geocoding with Bing
# ... are arguments passed from the geo() function
# https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/
batch_bing <- function(unique_addresses, lat = 'lat', long = 'long', timeout = 20, full_results = FALSE, custom_query = list(),
                       verbose = FALSE, api_url = NULL, ...) {
  # Specific endpoint
  if (is.null(api_url)) api_url <- 'http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode'
  
  address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters('bing', address_only = TRUE)]
  
  # filler result to return if needed
  NA_batch <- get_na_value(lat, long, rows = nrow(address_df))
  
  # Construct query ----
  # Bing needs a special list of params
  # https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/
  query_parameters <- get_api_query('bing',
                                    list(api_key = get_key('bing')),
                                    custom_parameters = list(input = 'pipe')
  )
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Create body of the POST request----
  # Needs to have Id and GeocodeRequest/Query 
  address_df <- dplyr::bind_cols(Id = seq_len(nrow(address_df)), address_df)
  names(address_df) <- c("Id","GeocodeRequest/Query")
  
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
    matrix(data="", ncol=length(response_fields), nrow = nrow(address_df))
    )
  names(mock) <- response_fields

  # Create tibble for body
  address_body <- dplyr::bind_cols(address_df, mock)
  
  # Create file
  body_file <- tempfile()
  cat("Bing Spatial Data Services, 2.0", file=body_file, append=FALSE, sep = "\n")
  cat(paste0(names(address_body), collapse = "|"), file=body_file, append=TRUE, sep = "\n")
  for (j in (seq_len(nrow(address_body)))){
    body <- paste0(address_body[j, ], collapse = "|")
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
  # Merge to original addresses and output----
  base <- tibble::as_tibble(address_body)
  results <- merge(base["Id"], 
                   result_parsed, 
                   all.x = TRUE)
  
  names(results)[names(results) == 'GeocodeResponse.Point.Latitude'] <- lat
  names(results)[names(results) == 'GeocodeResponse.Point.Longitude'] <- long
  
  if (full_results == FALSE) return(results[c(lat, long)])
  else return(cbind(results[c(lat, long)], results[!names(results) %in% c(lat, long)]))
}