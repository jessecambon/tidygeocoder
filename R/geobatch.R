

geobatch <- function(method, address_list,...) {
  switch(method, 
         "census" = census_batch(address_list,...),
         'geocodio'    = geocodio_batch(address_list,...)
  )
  
}

census_batch <- function(address_list, return = 'locations', timeout=5) {
  return_cols <- switch(return,
      'locations' = c('id', 'address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side'),
      'geographies' = c('id', 'address', 'status', 'quality', 'matched_address', 'coords', 'tiger_line_id', 'tiger_side', 'state_id', 'county_id', 'tract_id', 'block_id')
  )
  
  url_base <- paste0("https://geocoding.geo.census.gov/geocoder/", return, "/addressbatch")
  
  print(url_base)
    
  num_addresses <- length(address_list)
  NA_values <- rep("",num_addresses)
  
  # create 
  input_df <- tibble::tibble(
    id = 1:num_addresses,
    street = address_list,
    city = NA_values,
    state = NA_values,
    zip = NA_values,
  )
  
  # Write a Temporary CSV
  tmp <- tempfile(fileext = '.csv')
  utils::write.table(input_df, tmp, row.names = FALSE, col.names = FALSE, sep = ',', na = '')
  #check <- read.csv(tmp, header = FALSE)
  
  ## NOTE - request will fail if vintage and benchmark are invalid
  req <-
    httr::POST(url_base,
               body = list(
                 addressFile = httr::upload_file(tmp),
                 benchmark = 4,
                 vintage = 'ACS2019_Current',
                 format = 'json'
               ),
               encode = 'multipart',
               httr::timeout(60*timeout)
    )
  
  httr::stop_for_status(req)
  
  cnt <- httr::content(req, as = 'text', encoding = 'UTF-8')
  results <- utils::read.csv(text = cnt, header = FALSE,
                             col.names = return_cols,
                             fill = TRUE, stringsAsFactors = FALSE,
                             na.strings = '')
  ## TODO - split out lat/lng
  
  return(tibble::as_tibble(results))
}