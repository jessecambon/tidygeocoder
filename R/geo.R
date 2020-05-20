### Generic geocoder wrapper function. Returns results for ONE query.

# Query can be customized or left to default according to method (service) chosen. 

# full_results : if true return all info provided by geocoder (not just lat/long)

# Do not pass address if address = NULL ???

## method argument here will be used for:
# 1. setting the api_url and query options if they are not already set
# 2. figuring out how to parse the results that are returned (ie is it a dataframe? or list? field names? etc.)


## Set defaults for query parameters but allow user to override them. 
## Include address and limit as a query parameter.


geo <- function(method, api_url=NULL, query_parameters=list(), verbose=FALSE, min_time=NULL, full_results=FALSE) {
  start_time <- Sys.time() # start timer
  
  #### SET API URL BASED ON SERVICE IF IT IS NULL
  
  ### IF min_time is NULL then set it to 1 if method='osm', otherwise set to 0
  if (method == 'osm' & is.null(min_time))  min_time <- 1 else min_time <- 0


  ## Call Geocoder Service
  res <- get_raw_results(api_url, query_parameters)
  
  # If no results found, return NA
  if (length(res) == 0) {
    if (verbose == TRUE) {  message("No results found") }
    return(NA_value)
  }
  
  ## Parse geocoder results
  
  ## Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time,debug = debug) 
  
  ### Return  results
  
}