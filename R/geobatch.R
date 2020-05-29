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
batch_census <- function(address_list, return = 'locations', timeout=5) {
  return_cols <- switch(return,
      'locations' = c('id', 'address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side'),
      'geographies' = c('id', 'address', 'status', 'quality', 'matched_address',
    'coords', 'tiger_line_id', 'tiger_side', 'state_id', 'county_id', 'tract_id', 'block_id')
  )
  
  url_base <- get_census_url(return, 'addressbatch')
  
  print(url_base)
    
  num_addresses <- length(address_list)
  NA_values <- rep("",num_addresses)
  
  # create input dataframe
  input_df <- tibble::tibble(
    id = 1:num_addresses,
    street = address_list,
    city = NA_values,
    state = NA_values,
    zip = NA_values,
  )
  
  print(input_df)
  
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(input_df, tmp, row.names = FALSE, col.names = FALSE, sep = ',', na = '')
  #check <- read.csv(tmp, header = FALSE)
  
  ## NOTE - request will fail if vintage and benchmark are invalid for return = 'geographies'
  
  # Construct query
  query_parameters <- get_api_query('census', 
        custom_api_parameters = list(vintage = 'ACS2019_Current'))
  
  # Query API
  raw_content <- query_api(url_base, query_parameters, mode = 'file', 
          batch_file = tmp, content_encoding = "ISO-8859-1")

  results <- utils::read.csv(text = raw_content, header = FALSE,
                             col.names = return_cols,
                             fill = TRUE, stringsAsFactors = FALSE,
                             na.strings = '')
  
  ## split out lat/lng
  all_coordinates <- lapply(as.list(results$coords),split_coords)
  
  coord_df <- do.call('rbind', all_coordinates)
  colnames(coord_df) <- c('lat', 'long')
  
  # Combine extracted lat/longs with other return results
  combi <- cbind(subset(results, select = -coords),coord_df)
  
  # convert to tibble
  df_final <- tibble::as_tibble(combi)
  
  return(df_final)
}

#' Batch geocode with geocodio
#' @export
batch_geocodio <- function(address_list, timeout = 5) {
  url_base <- get_api_url('geocodio')
  # Construct query
  query_parameters <- get_api_query('geocodio', list(limit = 1, api_key = Sys.getenv('GEOCODIO_API_KEY')))
  
  # Query API
  raw_content <- query_api(url_base, query_parameters, mode = 'list', address_list = address_list)
  
  content <- jsonlite::fromJSON(raw_content,flatten = TRUE)
  
  results <- tibble::as_tibble(dplyr::bind_rows(content$results$response.results))
  
  return(results)
}
