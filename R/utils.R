## Put common utilities here

# Declaring global variables
pkg.globals <- new.env()
pkg.globals$address_arg_names <- c('address', 'street', 'city', 'county', 'state', 'postalcode', 'country')

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

# Extract latitude, longitude as a numeric vector. Not used for batch
# geocoding
# extract_coords <- function(method, response) {
#   lat_lng <- switch(method,
#   'census' = response$result$addressMatches$coordinates[c('y','x')],
#   'osm' = response[c('lat', 'lon')],
#   'iq' = response[c('lat', 'lon')],
#   'geocodio' = response$results$location[c('lat', 'lng')]
#   )
#   
#   if (length(lat_lng) == 0) lat_lng <- tibble::tibble(lat = NA, long = NA)
#   
#   names(lat_lng) <- c('lat','long')
#   lat_lng$lat <- as.numeric(lat_lng$lat)
#   lat_lng$long <- as.numeric(lat_lng$long)
#   
#   return(tibble::as_tibble(lat_lng))
# }

#' Extract geocoder results 
#' Not used for batch geocoding. Latitude and longitude and extracted into
#' the first two columns of the returned dataframe
#' @param method method name
#' @param response  content from the geocoder service (returned by the )
#' @param full_results if TRUE then the full results (not just latitude and longitude)
#'   will be returned.
#' @param flatten if TRUE then flatten any nested dataframe content
#' @export 
extract_results <- function(method, response, full_results = TRUE, flatten = TRUE) {
  
  NA_result <- tibble::tibble(lat = NA, long = NA)
  
  ## extract latitude and longitude as a dataframe
  lat_lng <- switch(method,
    'census' = response$result$addressMatches$coordinates[c('y','x')],
    'osm' = response[c('lat', 'lon')],
    'iq' = response[c('lat', 'lon')],
    'geocodio' = response$results$location[c('lat', 'lng')]
  )
  
  #print('lat_lng:')
  #print(lat_lng)
  
  # if null result then return NA
  if (length(lat_lng) == 0 ) return(NA_result)
  # check to make sure results aren't na or wrong width
  if (nrow(lat_lng) == 0 | ncol(lat_lng) != 2) return(NA_result)
  
  #print('checkpoint 1')
  # convert to numeric format
  #lat_lng <- tibble::as_tibble(sapply(lat_lng, function(x) as.numeric(as.character(x))))
  lat_lng[, 1] <- as.numeric(as.character(lat_lng[, 1]))
  lat_lng[, 2] <- as.numeric(as.character(lat_lng[, 2]))
  #print('checkpoint 2')
  
  
  if (full_results == TRUE) {
  ## extract full results excluding latitude and longitude
    results <- switch(method,
      'census' = response$result$addressMatches[!names(response$result$addressMatches) %in% c('coordinates')],
      'osm' = response[!names(response) %in% c('lat', 'lon')],
      'iq' =  response[!names(response) %in% c('lat', 'lon')],
      'geocodio' = response$results[!names(response$results) %in% c('location')]
    )
    
    combined_results <- tibble::as_tibble(cbind(lat_lng, results))
  } else {
    combined_results <- lat_lng
  }
  
  combined_results <- tibble::as_tibble(combined_results)
  
  #print('combined_results:')
  #print(combined_results)

  if (flatten == TRUE) return(jsonlite::flatten(combined_results))
  else return(combined_results)
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
  if (length(x) == 0) {
    filler_df <- data.frame(row.names = 1)

    for (col_name in column_names) {
      filler_df[col_name] <- NA
    }
    return(filler_df)
    
  } else return(x)
}

split_coords <- function(input) {
  ## Used by batch census function
  # input is a single character value. 
  # output is an unnamed numeric list with 2 elements: lat, long
  # if comma contained in input then split it. otherwise return NA list
  if (grepl(',', input, fixed = TRUE)) {
    split <- as.list(unlist(strsplit(input,",", fixed = TRUE)))
  }
  else split <- (list('',''))
  
  return(as.numeric(split))
}
