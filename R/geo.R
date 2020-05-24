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

#' Workhorse function for geocoding
#' @param method the geocoder function you want to use
#' \itemize{
#'   \item "census": US Census Geocoder. US street-level addresses only.
#'   \item "osm": Nominatim (OSM). Worldwide coverage.
#' }
#' @param address address to be geocoded
#' @return parsed results from geocoder
#' @export
geo <- function(method='census', address=NULL, api_key=NULL, limit=1, api_url=NULL, full_results=FALSE,
                verbose=FALSE, min_time=NULL) {
  start_time <- Sys.time() # start timer
  
  ### Set min_time if not set
  if (method %in% c('osm','iq') & is.null(min_time))  min_time <- 1 
  else if (is.null(min_time)) min_time <- 0

  ### Build Generic query ---------------------------
  generic_query <- list()
  if (!is.null(address)) generic_query[['address']] <- address
  if (!is.null(api_key)) generic_query[['api_key']] <- api_key
  if (!is.null(limit))   generic_query[['limit']]   <- limit
  
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method,generic_query)
  # define api url based on method
  
  # If api_url is not set then define it automatically.
  # If it's defined then figure out if its a URL (http...) 
  if (is.null(api_url)) api_url <- get_api_url(method) 
  else api_url <- get_api_url(method,url_name = trimws(api_url))
  
  ## TOdo --- implement custom URL field where user can specify the entire URL address
  #  else if (stringr::str_detect(stringr::str_trim(api_url),'^http'))  api_url <- api_url
  
  if (length(api_url) == 0) stop('API URL not found')
  
  raw_results <- query_api(api_url, api_query_parameters)
  #raw_results <- list()
  
  ##### TODO -------- make this return an NA_value
  # If no results found, return NULL
  if (length(raw_results) == 0) {
    if (verbose == TRUE) message("No results found")
    return(c())
  }
  
  ## Parse geocoder results
  coords <- extract_coords(method,raw_results)
  
  ## Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = TRUE) 
  
  ### Return  results
  return(coords)
}