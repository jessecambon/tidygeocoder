# Census batch geocoding
# @param address_pack packaged addresses object
# @param return should be 'locations' or 'geographies'
# Vingate must be defined if return = 'geographies'
# @export 
batch_census <- function(address_pack,
  return_type = 'locations', timeout = 20, full_results = FALSE, custom_query = list(), api_url = NULL,
  lat = 'lat', long = 'long', verbose = FALSE, ...) {
  
  #print('census batch return_type:')
  #print(return_type)
  
  if (!'street' %in% names(address_pack$unique) & (!'address' %in% names(address_pack$unique))) {
    stop("To use the census geocoder, either 'street' or 'address' must be defined")
  }
  
  location_cols <- c('id', 'input_address', 'match_indicator', 'match_type','matched_address', 
          'coords', 'tiger_line_id', 'tiger_side')
  return_cols <- switch(return_type,
          'locations' = location_cols,
          'geographies' = c(location_cols, c('state_fips', 'county_fips', 'census_tract', 'census_block'))
  )
  
  if (is.null(api_url)) api_url <- get_census_url(return_type, 'addressbatch')
  
  num_addresses <- nrow(address_pack$unique)
  if (verbose == TRUE) message(paste0('Number of Unique Addresses Passed to the Census Batch Geocoder: ', num_addresses))
  
  # create input dataframe
  input_df <- tibble::tibble(
    id      = 1:num_addresses,
    street  = if ('street' %in% names(address_pack$unique)) address_pack$unique$street else address_pack$unique$address,
    city    = if ('city' %in% names(address_pack$unique)) address_pack$unique$city else NA,
    state   = if ('state' %in% names(address_pack$unique)) address_pack$unique$state else NA,
    zip     = if ('postalcode' %in% names(address_pack$unique)) address_pack$unique$postalcode else NA
  )
  
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(input_df, tmp, row.names = FALSE, col.names = FALSE, sep = ',', na = '')
  
  # Construct query
  ## NOTE - request will fail if vintage and benchmark are invalid for return_type = 'geographies'
  query_parameters <- get_api_query('census', custom_parameters = custom_query)
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'file', 
          batch_file = tmp, content_encoding = "ISO-8859-1", timeout = timeout)

  results <- utils::read.csv(text = raw_content, header = FALSE,
                             col.names = return_cols,
                             fill = TRUE, stringsAsFactors = FALSE,
                             na.strings = '')

  ## split out lat/lng. lapply is used with as.numeric to convert coordinates to numeric
  coord_df <- do.call(rbind, lapply(strsplit(as.character(results$coords), ",", fixed = TRUE), as.numeric))
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
# @export
batch_geocodio <- function(address_pack, lat = 'lat', long = 'long', timeout = 20, full_results = FALSE, verbose = FALSE,
                           api_url = NULL, geocodio_v = 1.6, ...) {
  # https://www.geocod.io/docs/#batch-geocoding
  
  # limit the dataframe to legitimate arguments
  address_df <- address_pack$unique[names(address_pack$unique) %in% c('address', 'street', 'city', 'state', 'postalcode')]
  
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
  query_parameters <- get_api_query('geocodio', list(limit = 1, api_key = get_key('geocodio')))
  if (verbose == TRUE) display_query(api_url, query_parameters)
  
  # Query API
  raw_content <- query_api(api_url, query_parameters, mode = 'list', address_list = address_list, timeout = timeout)
  
  # Note that flatten here is necessary in order to get rid of the
  # nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
  content <- jsonlite::fromJSON(raw_content, flatten = TRUE)
  
  if ('address' %in% names(address_df)) {
    # results as a list of dataframes
    result_list <- content$results$response.results
  } else {
    # if address components were used we need to do a little more work to get
    # the results ...
    result_list <- lapply(content$results, function(x) x$response$results)
  }
  
  # if no results are returned then there is a 0 row dataframe in this
  # we need to replace this with a 1 row NA dataframe to preserve the number of rows
  result_list_filled <- lapply(result_list, filler_df, c('location.lat','location.long'))
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  results <- dplyr::bind_rows(result_list_filled)
  
  # rename lat/long columns
  names(results)[names(results) == 'location.lat'] <- lat
  names(results)[names(results) == 'location.lng'] <- long
  
  if (full_results == FALSE)  return(results[c(lat,long)])
  else return(cbind(results[c(lat,long)], results[!names(results) %in% c(lat,long)]))
}
