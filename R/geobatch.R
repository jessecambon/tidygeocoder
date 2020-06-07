# geobatch <- function(method, address_list,...) {
#   switch(method, 
#          "census"   = census_batch(address_list,...),
#          'geocodio' = geocodio_batch(address_list,...)
#   )
# }

#### Function for census batch geocoder
#### Accepts addresses as a vector
#' Census batch geocoding
#' Vingate must be defined if return = 'geographies'
#' @export 
batch_census <- function(address = NA, street = NA, city = NA, state = NA, postalcode = NA,
    return = 'locations', timeout=15, full_results = FALSE, verbose = FALSE, custom_query = list(),
                         lat = 'lat', long = 'long') {
  
  return_cols <- switch(return,
      'locations' = c('id', 'address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side'),
      'geographies' = c('id', 'address', 'status', 'quality', 'matched_address',
    'coords', 'tiger_line_id', 'tiger_side', 'state_id', 'county_id', 'tract_id', 'block_id')
  )
  
  url_base <- get_census_url(return, 'addressbatch')
    
  num_addresses <- max(sapply(list(address, street, city, state), length), na.rm = TRUE)

  if (verbose == TRUE) message(paste0('census batch geocoder, num_addresses: ', num_addresses))
  
  if (is.na(street)) street <- address
  
  # create input dataframe
  input_df <- tibble::tibble(
    id      = 1:num_addresses,
    street  = street,
    city    = city,
    state   = state,
    zip     = postalcode,
  )
  
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(input_df, tmp, row.names = FALSE, col.names = FALSE, sep = ',', na = '')
  #check <- read.csv(tmp, header = FALSE)
  
  ## NOTE - request will fail if vintage and benchmark are invalid for return = 'geographies'
  # Construct query
  query_parameters <- get_api_query('census', custom_api_parameters = custom_query)
  
  if (verbose == TRUE) display_query(url_base, query_parameters)
  
  # Query API
  raw_content <- query_api(url_base, query_parameters, mode = 'file', 
          batch_file = tmp, content_encoding = "ISO-8859-1", timeout = timeout)

  results <- utils::read.csv(text = raw_content, header = FALSE,
                             col.names = return_cols,
                             fill = TRUE, stringsAsFactors = FALSE,
                             na.strings = '')

  ## split out lat/lng. lapply is used with as.numeric to convert coordinates to numeric
  coord_df <- do.call(rbind, lapply(strsplit(as.character(results$coords),",", fixed = TRUE), as.numeric))
  colnames(coord_df) <- c(long, lat)  # <--- NOTE ORDER
  
  # convert to tibble and reorder coordinates
  coord_df <- tibble::as_tibble(coord_df)[c(lat, long)] 

  if (full_results == FALSE) return(coord_df)
  else {
    # Combine extracted lat/longs with other return results
    combi <- tibble::as_tibble(dplyr::bind_cols(dplyr::select(results,-coords), coord_df))
    
    return(combi)
  }
}

#' Batch geocode with geocodio
#' @export
batch_geocodio <- function(address_list, lat = 'lat', long = 'long', timeout = 5, full_results = FALSE) {
  url_base <- get_api_url('geocodio')
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = 1, api_key = get_key('geocodio')))
  
  # Query API
  raw_content <- query_api(url_base, query_parameters, mode = 'list', address_list = as.list(address_list))
  content <- jsonlite::fromJSON(raw_content,flatten = TRUE)
  
  # results as a list of dataframes
  result_list <- content$results$response.results
  
  # if no results are returned then there is a 0 row dataframe in this
  # we need to replace this with a 1 row NA dataframe to preserve the number of rows
  result_list_filled <- lapply(result_list,filler_df,c('location.lat','location.lng'))
  
  # combine list of dataframes into a single tibble. Column names may differ between the dataframes
  results <- dplyr::bind_rows(result_list_filled)
  
  # rename lat/long columns
  names(results)[names(results) == 'location.lat'] <- lat
  names(results)[names(results) == 'location.lng'] <- long
  
  if (full_results == FALSE)  return(results[c(lat,long)])
  else return(results)
}
