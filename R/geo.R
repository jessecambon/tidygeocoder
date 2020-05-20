### Generic geocoder wrapper function. Returns results for ONE query.

# Query can be customized or left to default according to method (service) chosen. 

# full_results : if true return all info provided by geocoder (not just lat/long)


# Do not pass address if address = NULL ???

## method argument here will be used for:
# 1. setting the api_url and query options if they are not already set
# 2. figuring out how to parse the results that are returned (ie is it a dataframe? or list? field names? etc.)


## Set defaults for query parameters but allow user to override them. 
## Include address and limit as a query parameter.

geo <- function(method,api_url=NULL, query_parameters=list(),
                lat=lat, long=long, verbose=FALSE, min_time=NULL, full_results=FALSE) {
  # what to return if address is invalid or no coordinates are found
  NA_value <- tibble::tibble(!!lat := numeric(), !!long := numeric())
  # lat/long variable names
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  #### SET API URL BASED ON SERVICE IF IT IS NULL
  
  ### IF min_time is NULL then set it to 1 if method='osm', otherwise set to 0
  if (method == 'osm' & is.null(min_time)) {
    min_time <- 1
  } else {
    min_time <- 0
  }
  
  ### If query
  
  
  start_time <- Sys.time() # start timer
  
  if (verbose == TRUE) { message(address)}
  
  # if address is NA or blank then return NA, else make call to Nominatim geocoder
  # numeric data is allowed but must be in string format (ie. for zip codes)
  if (is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
    return(NA_value)
  }
    
    ## Call Geocoder Service
    
    res <- get_raw_results(api_url, list(q = address, format = 'json', limit = 1 ))
    
    # If no results found, return NA
    if (length(res) == 0) {
      if (verbose == TRUE) {  message(paste("No results found for \"", address, "\".", sep = "")) }
      return(NA_value)
    }
    
    ## Parse geocoder results
    
    
    
    ## Make sure the proper amount of time has elapsed for the query per min_time
    
    seconds_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = 'secs'))
    
    if (debug == TRUE) message(paste0('Time elapsed: ', round(seconds_elapsed,1),' seconds'))
    
    # Sleep if necessary to make query take the minimum amount of time
    if (seconds_elapsed < min_time) {
      Sys.sleep(min_time - seconds_elapsed)
      
      if (debug == TRUE) message(paste0('Time elapsed (after sleep): ', 
                                        round(as.numeric(difftime(Sys.time(),start_time, units = 'secs')),1),' seconds'))
    }
    
    # Return  results
    if (!is.null(coords)) return(tibble::tibble(!!lat := coords[1], !!long := coords[2]))
    else return(NA_value)
  
}