## Put common utilities here

# remove a literal double quote from a string
# used with NSE
rm_quote <- function(string) gsub("\"","", string)

# How many seconds have elapsed since start time t0 (as defined by a t0 <- Sys.time() call) 
get_seconds_elapsed <- function(t0) {
  return(as.numeric(difftime(Sys.time(), t0, units = 'secs')))
}

# print time
print_time <- function(text, num_seconds) {
  message(paste0(text, ': ', round(num_seconds,1),' seconds'))
}

# Use Sys.sleep() to pause until a certain amount of time has elapsed
pause_until <- function(start_time,min_time,debug=FALSE) {
  ## Make sure the proper amount of time has elapsed for the query per min_time
  seconds_elapsed <- get_seconds_elapsed(start_time)
  if (debug == TRUE) print_time("Query completed in", seconds_elapsed)
  
  # Sleep if necessary to make query take the minimum amount of time
  if (seconds_elapsed < min_time) {
    Sys.sleep(min_time - seconds_elapsed)
    if (debug == TRUE) print_time("Total query time (including sleep)", get_seconds_elapsed(start_time))
  }
}

#' Extract latitude, longitude as as numeric vector c(lat, long)
#' response is the parsed json response
#' @export
extract_coords <- function(method, response) {
  lat_lng <- switch(method,
  'census' = unlist(response$result$addressMatches$coordinates[1,][c('y','x')], 
                use.names = FALSE),
  'osm' = as.numeric(c(response$lat, response$lon)),
  'iq' = as.numeric(c(response$lat, response$lon)),
  'geocodio' = c(response$results$location$lat, response$results$location$lng)
  )
  return(lat_lng)
}

#' extract results. exclude lat/long
#'@export
extract_results <- function(method, response, flatten = TRUE) {
  results <- switch(method,
  'census' = response$result$addressMatches[!names(response$result$addressMatches) %in% c('coordinates')][1,],
  'osm' = response[!names(response) %in% c('lat', 'lon')],
  'iq' =  response[!names(response) %in% c('lat', 'lon')],
  'geocodio' = response$results[!names(response$results) %in% c('location')]
  )
  
  if (flatten == TRUE) return(jsonlite::flatten(results))
  else return(results)
}

### Return a 2 column, 1 row NA tibble dataframe for coordinates that aren't found
# Given the column names (as strings)
get_na_value <- function(lat, long, rows = 1) {
  NA_df <- tibble::tibble(a = rep(NA, rows), b = rep(NA, rows))
  colnames(NA_df) <- c(lat, long)
  return(NA_df)
}

# For a list of dataframes, creates an NA df with 1 row with the column name supplied
# this is used in parsing the response of the geocodio batch geocoder
filler_df <- function(x, column_names) {
  if (nrow(x) == 0) {
    filler_df <- data.frame(row.names = 1)

    for (col_name in column_names) {
      filler_df[col_name] <- NA
    }
    return(filler_df)
    
  } else return(x)
}


make_component_list <- function(address_df) {
  # Accepts a dataframe of unique addresses
  #(address_pack$unique from package_addresses() function) and returns
  # a nested list to be used by the geocodio batch query
  
  addresses <- list()
  for (index in 1:nrow(address_pack)) {
    addresses[[index]] <- as.list(address_pack[index,])
  }
  names(addresses) <- 1:nrow(address_pack)
  return(addresses)
}
