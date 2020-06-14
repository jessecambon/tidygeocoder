#' Geocode addresses that are passed as character value or vectors
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
#' @param method the geocoder function you want to use
#' \itemize{
#'   \item "census": US Census Geocoder. US street-level addresses only.
#'   \item "osm": Nominatim (OSM). Worldwide coverage.
#'   \item "iq": Commercial OSM geocoder service.
#'   \item "geocodio": Commercial geocoder. Covers US and Canada. 
#' }
#' @param lat latitude column name
#' @param long longitude column name
#' @param limit number of results to return per address
#' @param min_time minimum amount of time for a query to take in seconds.
#'  This parameter is used to abide by API usage limits. Not used in batch geocoding
#' @param api_url Custom API URL
#' @param timeout query timeout (in minutes)
#' 
#' @param mode method of geocoding used. 'auto' (default), 'batch', or 'single'. 
#'    Auto uses batch geocoding if there are multiple addresses
#' @param full_results returns all data from API if TRUE
#' @param unique_only only return unique results if TRUE
#' @param return_addresses return input addresses with results
#' 
#' @param flatten if TRUE then any nested dataframes in results are flattened
#' @param batch_limit limit to the number of addresses in a batch 
#' @param verbose toggles console output. useful for debugging
#' @param no_query if TRUE then no queries are sent to the geocoder and verbose is set to TRUE

#' @param custom_query API-specific parameters to be used
#' @param return    (census only) 'locations' (default) or 'geographies'
#' @param iq_region 'us' (default) or 'eu'. Used for establishing API URL
#' @param geocodio_v version of geocodio api. 1.6 is default. used for establishing API URL
#' 
#' @return parsed results from geocoder
#' @export
geo <- function(address = NULL, 
    street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL,
    method = 'census', lat = lat, long = long, limit=1, min_time=NULL, api_url = NULL, timeout = 20,
    mode = 'auto', full_results = FALSE, unique_only = FALSE, return_addresses = TRUE, 
    flatten = TRUE, batch_limit = 10000, verbose = FALSE, no_query = FALSE, 
    custom_query = list(), return = 'locations', iq_region = 'us', geocodio_v = 1.6) {
  
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
  if (method == 'cascade') return(do.call(geo_cascade, all_args[names(all_args) != 'method']))
  
  # check address inputs and deduplicate
  address_pack <- package_addresses(address, street, city , county, 
           state, postalcode, country)
  
  # count number of unique addresses
  num_unique_addresses <- nrow(address_pack$unique) # unique addresses
  # determine how many rows will be returned (either unique or includes duplicate address rows)
  num_rows_to_return <- ifelse(unique_only, num_unique_addresses, nrow(address_pack$crosswalk))
  
  if (verbose == TRUE) message(paste0('Number of Unique Addresses: ', num_unique_addresses))
  
  # If no valid/nonblank addresses are passed then return NA
  if (num_unique_addresses == 0) {
    if (verbose == TRUE) message(paste0('No non-blank non-NA addreses found. Returning NA results.'))
    return(get_na_value(lat, long, rows = num_rows_to_return))
  }
  ### if address(es) is/are blank then return NA (should no longer be needed)
  # if  (max(sapply(address_pack$unique, nchar, allowNA = TRUE)) %in% c(0, NA)) {
  #   if (verbose == TRUE) message("Blank or missing address!")
  #   return(get_na_value(lat, long, rows = num_rows_to_return))
  # }
  
  ## If there are multiple addresses and we are using a method without a batch geocoder 
  ## OR the user has explicitly specified single address geocoding.. call the 
  ## single address geocoder in a loop
  if ((num_unique_addresses > 1) & ((method %in% c('osm', 'iq')) | (mode == 'single'))) {
      if (verbose == TRUE) message('Executing single address geocoding...\n')
      
      # construct args for single address query
      # note that non-address related fields go to the MoreArgs argument of mapply
      # since we aren't iterating through them
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
      return(unpackage_addresses(address_pack, stacked_results, unique_only, return_addresses))
      }
      
      ### Batch geocoding -----------------------------------------------------
      if ((num_unique_addresses > 1) | (mode == 'batch')) {
      
      #### Enforce batch limit
      if (num_unique_addresses > batch_limit) {
        message(paste0('Limiting batch query to ', batch_limit, ' addresses'))
        address_pack$unique <- address_pack$unique[1:batch_limit, ]
      }
        
      if (verbose == TRUE) message(paste0('Calling the ', method, ' batch geocoder'))
      # Convert our generic query parameters into parameters specific to our API (method)
      if (no_query == TRUE) return(get_na_value(lat, long, rows = num_rows_to_return))
      
        
      raw_results <- switch(method,
        'census' = do.call(batch_census, c(list(address_pack),
              all_args[!names(all_args) %in% address_arg_names])),
        'geocodio' = do.call(batch_geocodio, c(list(address_pack),
              all_args[!names(all_args) %in% address_arg_names]))
        )
      
      # map the raw results back to the original addresses that were passed if there are duplicates
      return(unpackage_addresses(address_pack, raw_results, unique_only, return_addresses))
    }
  
  #################################################################
  #### Code past this point is for geocoding a single address #####
  #################################################################
  
  # what to return when we don't find results
  NA_value <- get_na_value(lat, long)
  
  ### Set min_time if not set
  if (method %in% c('osm','iq') & is.null(min_time))  min_time <- 1 
  else if (is.null(min_time)) min_time <- 0
  
  ### Start to build 'generic' query as named list -------------------------
  generic_query <- list()
  ## Geocodio and IQ services require an API key
  if (method %in% c('geocodio','iq')) {
    generic_query[['api_key']] <- get_key(method)
  }
  if (!is.null(limit)) generic_query[['limit']]   <- limit
  
  # construct query with single-line address or address components
  if  (!is.null(address)) {
    search <- 'onelineaddress' # for census only
    generic_query[['address']] <- address_pack$unique$address
  }
  else {
    search <- 'address' # for census only
    if (!is.null(street))       generic_query[['street']]       <- address_pack$unique$street
    if (!is.null(city))         generic_query[['city']]         <- address_pack$unique$city
    if (!is.null(county))       generic_query[['county']]       <- address_pack$unique$county
    if (!is.null(state))        generic_query[['state']]        <- address_pack$unique$state
    if (!is.null(postalcode))   generic_query[['postalcode']]   <- address_pack$unique$postalcode
    if (!is.null(country))      generic_query[['country']]      <- address_pack$unique$country
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
  
  if (length(api_url) == 0) stop('API URL not found')
  
  ### Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  if (no_query == TRUE) return(NA_value)
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  # If no results found, return NA
  if (length(raw_results) == 0) {
    if (verbose == TRUE) message("No results found")
    return(unpackage_addresses(address_pack, NA_value, unique_only, return_addresses))
  }
  
  ### Extract lat/long as an unnamed numeric vector c(lat,long)
  coords <- extract_coords(method, raw_results)
  
  if (length(coords) == 0) {
    if (verbose == TRUE) message("No results found")
    return(unpackage_addresses(address_pack, NA_value, unique_only, return_addresses))
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
    return(unpackage_addresses(address_pack, complete_results, unique_only, return_addresses))
  } else return(unpackage_addresses(address_pack, coords_tibble, unique_only, return_addresses))
}