# Query can be customized or left to default according to method (service) chosen. 
# full_results : if true return all info provided by geocoder (not just lat/long)
## Set defaults for query parameters but allow user to override them. 
## Include address and limit as a query parameter.
# Either address of query_parameters must be defined

#' Workhorse function for geocoding
#' @param method the geocoder function you want to use
#' \itemize{
#'   \item "census": US Census Geocoder. US street-level addresses only.
#'   \item "osm": Nominatim (OSM). Worldwide coverage.
#' }
#' @param address single line address. do not combine with address component arguments below
#' 
#' Address components (do not combine with the single line 'address' parameter)
#' @param street street address
#' @param city city
#' @param county county
#' @param state state
#' @param postalcode postalcode (zip code if in the United States)
#' @param country country
#' 
#' @param return    (census only) 'locations' (default) or 'geographies'
#' 
#' @param api_url Custom URL to use for API. Overrides default URL
#' @param custom_query API-specific parameters to be used
#' 
#' @param full_results returns all data from API if TRUE
#' @param unique_only only return unique results if TRUE
#' @param flatten if TRUE then any nested dataframes in results are flattened
#' 
#' @param iq_region 'us' (default) or 'eu'
#' @param geocodio_v 1.6
#' 
#' @param no_query if TRUE then no queries are sent to the geocoder and verbose is set to TRUE
#' @return parsed results from geocoder
#' @export
geo <- function(address = NULL, 
    street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL,
    method = 'census', lat = lat, long = long, limit=1, api_url = NULL, return = 'locations', 
    custom_query = list(), full_results = FALSE, unique_only = FALSE, flatten = TRUE,
    verbose = FALSE, min_time=NULL, no_query = FALSE, iq_region = 'us', geocodio_v = 1.6) {
  
  # NSE - Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # capture all function arguments including default values as a named list.
  # make sure to put this before any other variables are defined
  all_args <- as.list(environment())
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  # names of all address fields
  address_arg_names <- c('address', 'street', 'city', 'county', 'state', 'postalcode', 'country')
  
  # If method='cascade' is called then pass all function arguments 
  # except for method to geo_cascade and return the results 
  if (method == 'cascade') return(do.call(geo_cascade,all_args[names(all_args) != 'method']))
  
  # check address inputs and deduplicate
  address_pack <- package_addresses(address, street, city , county, 
           state, postalcode, country)
  
  num_addresses <- nrow(address_pack$unique)
  if (verbose == TRUE) message(paste0('Number of Addresses: ', num_addresses))
  
  ### Set min_time if not set
  if (method %in% c('osm','iq') & is.null(min_time))  min_time <- 1 
  else if (is.null(min_time)) min_time <- 0
  
  ### Build Generic query as named list ---------------------------
  generic_query <- list()
  if (method %in% c('geocodio','iq')) {
    generic_query[['api_key']] <- get_key(method)
    if (generic_query[['api_key']] == '') stop("API Key must be defined")
  }
  if (!is.null(limit))                generic_query[['limit']]   <- limit
  
  ### If there are multiple addresses then we either call the geo function
  ### repeatedly and aggregate the results OR we call a batch geocoder function
  if (num_addresses > 1) {
    if (method %in% c('osm', 'iq')) {
      if (verbose == TRUE) message('Executing single address geocoding...\n')
      
      # construct args for single address query
      # note that non-address related fields go to the MoreArgs argument of mapply
      # since we aren't interating through them
      single_addr_args <- c(
        list(FUN = geo), 
        as.list(address_pack$unique),
        list(MoreArgs = all_args[!names(all_args) %in% address_arg_names],
          USE.NAMES = FALSE, SIMPLIFY = FALSE)
      )
      # remove NULL and 0 length items  <--- Possibly uneccessary now?
      single_addr_args <- single_addr_args[sapply(single_addr_args, length, USE.NAMES = FALSE) != 0]  
      
      # Geocode each address individually by recalling this function with mapply
      list_coords <- do.call(mapply, single_addr_args)
      # rbind the list of tibble dataframes together
      stacked_results <- dplyr::bind_rows(list_coords)
      return(unpackage_addresses(address_pack, stacked_results, unique_only))
      
    } else {
      ### Call Batch Geocoding
      if (verbose == TRUE) message(paste0('Calling the ', method, 'batch geocoder'))
      # Convert our generic query parameters into parameters specific to our API (method)
      if (no_query == TRUE) return(get_na_value(lat, long, rows = num_addresses))
      
      raw_results <- switch(method,
        'census' = do.call(batch_census, c(list(address_pack),
              all_args[!names(all_args) %in% address_arg_names])),
        'geocodio' = do.call(batch_geocodio, c(list(address_pack),
              all_args[!names(all_args) %in% address_arg_names]))
        )
      
      # map the raw results back to the original addresses that were passed if there are duplicates
      return(unpackage_addresses(address_pack, raw_results, unique_only))
    }
  }
  
  #################################################################
  #### Code past this point is for geocoding a single address #####
  #################################################################
  
  # what to return when we don't find results
  NA_value <- get_na_value(lat,long)
  
  # construct query with single-line address or address components
  if  (!is.null(address)) {
    search <- 'onelineaddress' # for census only
    generic_query[['address']]      <- address
  }
  else {
    search <- 'address' # for census only
    if (!is.null(street))       generic_query[['street']]       <- street
    if (!is.null(city))         generic_query[['city']]         <- city
    if (!is.null(county))       generic_query[['county']]       <- county
    if (!is.null(state))        generic_query[['state']]        <- state
    if (!is.null(postalcode))   generic_query[['postalcode']]   <- postalcode
    if (!is.null(country))      generic_query[['country']]      <- country
  }
  
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method, generic_query, custom_query)
  
  # Set API URL (if not already set) ---------------------------
  if (is.null(api_url)) {
    api_url <- switch(method,
      "census" = get_census_url(return, search),
      "osm" = get_osm_url(),
      "geocodio" = get_geocodio_url(geocodio_v),
      "iq" = get_iq_url(iq_region)
      )
  }
  
  if (length(api_url) == 0) {
    warning('API URL not found')
    return(NA_value)
  }
  
  ### Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  if (no_query == TRUE) return(NA_value)
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  # If no results found, return NA
  if (length(raw_results) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NA_value)
  }
  
  ### Extract lat/long as an unnamed numeric vector c(lat,long)
  coords <- extract_coords(method, raw_results)
  
  if (length(coords) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NA_value)
  }
  
  # Convert numeric vector to tibble
  names(coords) <- c(lat, long)
  coords_tibble <- tibble::as_tibble_row(coords)
  
  ### Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  
  if (full_results == TRUE) {
    # extract result details (doesn't include coordinates)
    result_details <- extract_results(method, raw_results, flatten)
    complete_results <- tibble::as_tibble(dplyr::bind_cols(coords_tibble, result_details))
    return(complete_results)
  } else return(coords_tibble)
}