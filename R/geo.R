#' Geocode addresses
#' 
#' @description
#' Geocodes addresses given as character values. The \code{\link{geocode}}
#' function utilizes this function on addresses contained in dataframes.
#' See example usage in \code{vignette("tidygeocoder")} 
#' 
#' Note that not all geocoder services support certain address component 
#' parameters. For example, the Census geocoder only covers the United States 
#' and does not have a "country" parameter. Refer to \code{\link{api_parameter_reference}} 
#' for more details on geocoder services and API usage. 
#' 
#' This function uses the \code{\link{get_api_query}}, \code{\link{query_api}}, and
#' \code{\link{extract_results}} functions to create, execute, and parse the geocoder
#' API queries.
#' 
#' @param address single line address (ie. '1600 Pennsylvania Ave NW, Washington, DC').
#'    Do not combine with the address component arguments below
#'    (street, city , county, state, postalcode, country).
#' @param street street address (ie. '1600 Pennsylvania Ave NW')
#' @param city city (ie. 'Tokyo')
#' @param county county (ie. 'Jefferson')
#' @param state state (ie. 'Kentucky')
#' @param postalcode postalcode (zip code if in the United States)
#' @param country country (ie. 'Japan')
#' 
#' @param method the geocoder service to be used. Refer to 
#' `api_parameter_reference` and the API documentation for
#' each geocoder service for usage details and limitations.
#' \itemize{
#'   \item \code{"census"}: US Census Geocoder. US street-level addresses only. 
#'      Can perform batch geocoding.
#'   \item \code{"osm"}: Nominatim (OSM). Worldwide coverage.
#'   \item \code{"geocodio"}: Commercial geocoder. Covers US and Canada and has
#'      batch geocoding capabilities. Requires an API Key to be stored in
#'      the "GEOCODIO_API_KEY" environmental variable.
#'   \item \code{"iq"}: Commercial Nominatim geocoder service. Requires an API Key to
#'      be stored in the "LOCATIONIQ_API_KEY" environmental variable.
#'   \item \code{"cascade"} : Attempts to use one geocoder service and then uses
#'     a second geocoder service if the first service didn't return results.
#'     The services and order is specified by the cascade_order argument. 
#'     Note that this is not compatible with \code{full_results = TRUE} as geocoder
#'     services have different columns that they return.
#' }
#' @param cascade_order a vector with two character values for the method argument 
#' in the order in which the geocoder services will be attempted for method = "cascade"
#' @param lat latitude column name. Can be quoted or unquoted (ie. lat or 'lat').
#' @param long longitude column name. Can be quoted or unquoted (ie. long or 'long').
#' @param limit number of results to return per address. Note that 
#'  limit > 1 is not compatible with batch geocoding if return_addresses = TRUE.
#' @param min_time minimum amount of time for a query to take (in seconds) if using
#'  Location IQ or OSM. This parameter is used to abide by API usage limits. You can
#'  set it to a lower value (ie. 0) if using a local Nominatim server, for instance.
#' @param api_url Custom API URL. If specified, the default API URL will be overridden.
#'  This can be used to specify a local Nominatim server.
#' @param timeout query timeout (in minutes)
#' 
#' @param mode set to 'batch' to force batch geocoding and 'single' to 
#'  force single address geocoding (one address per query). If not 
#'  specified then batch geocoding will be used if available
#'  (given method selected) when multiple addresses are provided, otherwise
#'  single address geocoding will be used.
#' @param full_results returns all data from geocoder service if TRUE
#' @param unique_only only return results for unique addresses if TRUE
#' @param return_addresses return input addresses with results if TRUE
#' 
#' @param flatten if TRUE then any nested dataframes in results are flattened
#' @param batch_limit limit to the number of addresses in a batch geocoding query.
#'  Both geocodio and census batch geocoders have a 10,000 address limit so this
#'  is the default.
#' @param verbose if TRUE then detailed logs are output to the console
#' @param no_query if TRUE then no queries are sent to the geocoder and verbose is set to TRUE 

#' @param custom_query API-specific parameters to be used, passed as a named list 
#'  (ie. `list(vintage = 'Current_Census2010')`)
#' @param return_type    (census only) 'locations' (default) or 'geographies' which 
#'  returns additional census geography columns. See the Census geocoder API 
#'  documentation for more details.
#' @param iq_region 'us' (default) or 'eu'. Used for establishing API URL for the 'iq' method
#' @param geocodio_v version of geocodio api. 1.6 is default. Used for establishing API URL
#'   for the 'geocodio' method.
#' 
#' @return parsed results from the geocoder service
#' @examples
#' \donttest{
#' 
#' geo(street = "600 Peachtree Street NE", city = "Atlanta",
#'  state = "Georgia", method = "census")
#' 
#' geo(address = c("Tokyo, Japan", "Lima, Peru", "Nairobi, Kenya"),
#'  method = 'osm')
#' 
#' geo(county = 'Jefferson', state = "Kentucky", country = "US",
#'      method = 'osm')
#' 
#' }
#' @seealso \code{\link{api_parameter_reference}}
#' @export
geo <- function(address = NULL, 
    street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL,
    method = 'census', cascade_order = c('census', 'osm'), lat = lat, long = long, limit = 1, 
    min_time = NULL, api_url = NULL, timeout = 20,
    mode = '', full_results = FALSE, unique_only = FALSE, return_addresses = TRUE, 
    flatten = TRUE, batch_limit = 10000, verbose = FALSE, no_query = FALSE, 
    custom_query = list(), return_type = 'locations', iq_region = 'us', geocodio_v = 1.6) {

  # NSE - Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # capture all function arguments including default values as a named list.
  # make sure to put this before any other variables are defined
  all_args <- as.list(environment())
  
  ## Check inputs
  stopifnot(mode %in% c('', 'single', 'batch'), 
    method %in% c('census', 'osm', 'iq', 'geocodio', 'cascade'),
    is.logical(verbose), is.logical(no_query), is.logical(flatten), 
     is.logical(full_results), is.logical(unique_only), is.logical(return_addresses), 
     limit >= 1, batch_limit >= 1)
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  # If method='cascade' is called then pass all function arguments 
  # except for method to geo_cascade and return the results 
  if (method == 'cascade') {
    if (full_results == TRUE) stop("full_results = TRUE cannot be used with the cascade method.")
    if (limit != 1) stop("limit argument must be 1 (default) to use the cascade method.")
    return(do.call(geo_cascade, all_args[!names(all_args) %in% c('method')]))
  }
  
  # check address inputs and deduplicate
  address_pack <- package_addresses(address, street, city , county, 
           state, postalcode, country)
  
  # count number of unique addresses
  num_unique_addresses <- nrow(address_pack$unique) # unique addresses
  # determine how many rows will be returned (either unique or includes duplicate address rows)
  num_rows_to_return <- ifelse(unique_only, num_unique_addresses, nrow(address_pack$crosswalk))
  
  NA_value <- get_na_value(lat, long, rows = num_rows_to_return) # filler result to return if needed
  
  if (verbose == TRUE) message(paste0('Number of Unique Addresses: ', num_unique_addresses))
  
  # If no valid/nonblank addresses are passed then return NA
  if (num_unique_addresses == 1 & all(is.na(address_pack$unique))) {
    if (verbose == TRUE) message(paste0('No non-blank non-NA addreses found. Returning NA results.'))
    return(unpackage_addresses(address_pack, NA_value, unique_only, return_addresses))
  }
  
  ## If there are multiple addresses and we are using a method without a batch geocoder 
  ## OR the user has explicitly specified single address geocoding.. call the 
  ## single address geocoder in a loop
  if ((num_unique_addresses > 1) & ((method %in% c('osm', 'iq')) | (mode == 'single'))) {
      if (verbose == TRUE) {
        message('Executing single address geocoding...')
        message()
      }
      
      # construct args for single address query
      # note that non-address related fields go to the MoreArgs argument of mapply
      # since we aren't iterating through them
      single_addr_args <- c(
        list(FUN = geo), 
        as.list(address_pack$unique),
        list(MoreArgs = all_args[!names(all_args) %in% pkg.globals$address_arg_names],
          USE.NAMES = FALSE, SIMPLIFY = FALSE)
      )
      
      # Geocode each address individually by recalling this function with mapply
      list_coords <- do.call(mapply, single_addr_args)
      # rbind the list of tibble dataframes together
      stacked_results <- dplyr::bind_rows(list_coords)
      # note that return_addresses has been set to FALSE here since addresses will already
      # be returned in the first geo function call (if asked for)
      return(unpackage_addresses(address_pack, stacked_results, unique_only, return_addresses = FALSE))
      }
      
  ### Batch geocoding -----------------------------------------------------
  if ((num_unique_addresses > 1) | (mode == 'batch')) {
    
    if (limit != 1 & return_addresses == TRUE) {
    stop('For batch geocoding (more than one address per query) the limit argument must 
    be 1 (default) OR the return_addresses argument must be FALSE. Possible solutions:
    1) Set the mode argument to "single" to force single (not batch) geocoding 
    2) Set limit argument to 1 (ie. 1 result is returned per address)
    3) Set return_addresses to FALSE
    See the geo() function documentation for details.')
    }
  
    #### Enforce batch limit
    if (num_unique_addresses > batch_limit) {
      message(paste0('Limiting batch query to ', batch_limit, ' addresses'))
      address_pack$unique <- address_pack$unique[1:batch_limit, ]
      num_rows_to_return <- batch_limit
    }
      
    if (verbose == TRUE) message(paste0('Passing ', num_unique_addresses, 
                          ' addresses to the ', method, ' batch geocoder'))
    # Convert our generic query parameters into parameters specific to our API (method)
    if (no_query == TRUE) return(unpackage_addresses(address_pack, 
         get_na_value(lat, long, rows = num_rows_to_return), 
         unique_only, return_addresses))
    
    batch_results <- switch(method,
      'census' = do.call(batch_census, c(list(address_pack),
            all_args[!names(all_args) %in% pkg.globals$address_arg_names])),
      'geocodio' = do.call(batch_geocodio, c(list(address_pack),
            all_args[!names(all_args) %in% pkg.globals$address_arg_names]))
      )
    
    # if verbose = TRUE, tell user how long batch query took
    if (verbose == TRUE) {
      batch_time_elapsed <- get_seconds_elapsed(start_time)
      print_time("Query completed in", batch_time_elapsed)
    }
    
    # map the raw results back to the original addresses that were passed if there are duplicates
    return(unpackage_addresses(address_pack, batch_results, unique_only, return_addresses))
  }

  #################################################################
  #### Code past this point is for geocoding a single address #####
  #################################################################
  
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
    search <- 'onelineaddress' # for census API URL 
    generic_query[['address']] <- address_pack$unique$address
  }
  else {
    search <- 'address' # for census API URL
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
      "census" = get_census_url(return_type, search),
      "osm" = get_osm_url(),
      "geocodio" = get_geocodio_url(geocodio_v),
      "iq" = get_iq_url(iq_region)
      )
  }
  
  if (length(api_url) == 0) stop('API URL not found')
  
  ### Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  if (no_query == TRUE) return(unpackage_addresses(address_pack, NA_value, unique_only, return_addresses))
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  ## output error message for geocodio if present
  if ((method == 'geocodio') & (!is.data.frame(raw_results)) & ("error" %in% names(raw_results))) {
    message(paste0('Error: ', raw_results$error))
    results <- NA_value
  } 
  else if (length(raw_results) == 0) {
    # If no results found, return NA
    # otherwise extract results
    results <- NA_value
    if (verbose == TRUE) message("No results found")
  } else {
    ### Extract results. Use the full_results and flatten parameters
    ### to control the output
    results <- extract_results(method, raw_results, full_results, flatten)
    
    # Name the latitude and longitude columns in accordance with lat/long arguments
    names(results)[1] <- lat
    names(results)[2] <- long
  }
  
  ### Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  if (verbose == TRUE) message() # insert ending line break if verbose
  
  return(unpackage_addresses(address_pack, results, unique_only, return_addresses))
}
