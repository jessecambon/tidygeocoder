### Generic geocoder wrapper function. Returns results for ONE query.

# Query can be customized or left to default according to method (service) chosen. 

# full_results : if true return all info provided by geocoder (not just lat/long)

# Do not pass address if address = NULL ???

## method argument here will be used for:
# 1. setting the api_url and query options if they are not already set
# 2. figuring out how to parse the results that are returned (ie is it a dataframe? or list? field names? etc.)


## Set defaults for query parameters but allow user to override them. 
## Include address and limit as a query parameter.

# Either address of query_parameters must be defined

geo <- function(method, address=NULL, api_url=NULL, api_key=NULL, limit=NULL, query_parameters=NULL, 
          verbose=FALSE, min_time=NULL, full_results=FALSE) {
  start_time <- Sys.time() # start timer
  
  ### Set min_time if not set
  if (method %in% c('osm','iq') & is.null(min_time))  min_time <- 1 
  else if (is.null(min_time)) min_time <- 0

  # if no query_parameters are passed then define them by defaults
  query_parameters <- get_address_query(method,address,api_key,limit)
  # define api url based on method
  
  
  if (is.null(api_url)) api_url <- get_api_url(method) 
  else if (stringr::str_detect(str_trim(api_url),'^http'))  api_url <- api_url
  else api_url <- get_api_url(method,url_name = api_url)

  
  ## Call Geocoder Service
  message(paste0('API URL: ', api_url))
  mesage(paste0("Query: ", query_parameters))
  
  #raw_results <- query_api(api_url, query_parameters)
  
  # If no results found, return NA
  if (length(raw_results) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NULL)
  }
  
  ## Parse geocoder results
  
  ## Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = TRUE) 
  
  ### Return  results
  return(raw_results)
}