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
geo <- function(address=NULL, method='census', lat = lat, long = long,
    api_key=NULL, limit=1, api_url=NULL, custom_query=list(),
    full_results=FALSE, verbose=FALSE, min_time=NULL, debug = FALSE) {
  # debug turns on all messaging
  if (debug == TRUE) verbose <- TRUE
  
  ### Set min_time if not set
  if (method %in% c('osm','iq') & is.null(min_time))  min_time <- 1 
  else if (is.null(min_time)) min_time <- 0
  
  # NSE - Quoted unquoted vars without double quoting quoted vars
  lat <- gsub("\"","", deparse(substitute(lat)))
  long <- gsub("\"","", deparse(substitute(long)))
  
  start_time <- Sys.time() # start timer
  # what to return when we don't find results
  NA_value <- get_na_value(lat,long)
  
  ### Build Generic query as named list ---------------------------
  generic_query <- list()
  if (!is.null(api_key)) generic_query[['api_key']] <- api_key
  if (!is.null(limit))   generic_query[['limit']]   <- limit
  
  
  ### If more than one adress is passed then use batch mode if available
  if ((length(c(address)) > 1)) {
    if (!method %in% c('census', 'geocodio')) {
      message(paste0('Batch geocoding not available for ', method, '. Pass a single address.'))
    } else {
      # Convert our generic query parameters into parameters specific to our API (method)
      api_query_parameters <- get_api_query(method,generic_query)
      return(switch(method,
                    'census' = batch_census(c(address), full_results = full_results),
                    'geocodio' = batch_geocodio(c(address), full_results = full_results)
                    ))
    }
  }
  
  if (!is.null(address)) generic_query[['address']] <- address
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method,generic_query)
  
  # If cascade then call geo_cascade which will then call this function once or twice
  if (method == 'cascade') return(geo_cascade(address = address, lat = lat, long = long, 
        limit = limit, api_url = api_url, custom_query = custom_query))
  
  # If there is no custom query then we are relying on the address and it must
  # be non-missing/NA
  
  # Check if address NULL
  if ((length(custom_query) == 0) & is.null(address)) {
    if (verbose == TRUE) message("Blank or missing address!")
    return(NA_value)    
  }
  
  # Check if address is missing/NA
  if ((length(custom_query) == 0) & (is.na(address) | trimws(address) == "")) {
    if (verbose == TRUE) message("Blank or missing address!")
    return(NA_value)
  }
  
  
  # If api_url is not set then define it automatically.
  # If it's defined then figure out if its a URL (http...) 
  if (is.null(api_url)) api_url <- get_api_url(method) 
  else api_url <- get_api_url(method,url_name = trimws(api_url))
  
  ## TOdo --- implement custom URL field where user can specify the entire URL address
  #  else if (stringr::str_detect(stringr::str_trim(api_url),'^http'))  api_url <- api_url
  
  if (length(api_url) == 0) {
    warning('API URL not found')
    return(NA_value)
  }
  
  ### Execute Single Address Query -----------------------------------------
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  # If no results found, return NA
  if (length(raw_results) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NA_value)
  }
  
  ### Extract lat/long as an unnamed numeric vector c(lat,long)
  coords <- extract_coords(method,raw_results)
  
  if (length(coords) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NA_value)
  }
  
  # Convert numeric vector to tibble
  names(coords) <- c(lat, long)
  coords_tibble <- tibble::as_tibble_row(coords)
  
  ### Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  
  ### Return  results
  return(coords_tibble)
}