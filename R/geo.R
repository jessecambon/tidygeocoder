
## IMPORTANT: All new batch geocoding functions must be added to batch_func_map
# the geo() function references this list to find batch geocoding functions (batch_geocoding.R)
# maps method names to batch functions
batch_func_map <- list(
  geocodio = batch_geocodio, 
  census = batch_census
)

# stores the minimum number of seconds that must elapse for each query
# based on the usage limit of the service (free tier if there are multiple plans available)
# Stored value is SECONDS PER QUERY
usage_limit_map <- list(
  osm = 1,               # 1 query/second             
  geocodio = 60/1000,    # 1000 queries per minute (free tier)
  iq = 1/2,              # 2 queries per second (free tier)
  google = 1/50          # 50 queries per second
)

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
#' for more details on geocoder service parameters and API usage. 
#' 
#' This function uses the \code{\link{get_api_query}}, \code{\link{query_api}}, and
#' \code{\link{extract_results}} functions to create, execute, and parse the geocoder
#' API queries.
#' 
#' @param address single line address (ie. '1600 Pennsylvania Ave NW, Washington, DC').
#'    Do not combine with the address component arguments below
#'    (street, city, county, state, postalcode, country).
#' @param street street address (ie. '1600 Pennsylvania Ave NW')
#' @param city city (ie. 'Tokyo')
#' @param county county (ie. 'Jefferson')
#' @param state state (ie. 'Kentucky')
#' @param postalcode postalcode (zip code if in the United States)
#' @param country country (ie. 'Japan')
#' 
#' @param method the geocoder service to be used. Refer to 
#' \code{\link{api_parameter_reference}} and the API documentation for
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
#'   \item \code{"google"}: Commercial Google geocoder service. Requires an API Key to
#'      be stored in the "GOOGLEGEOCODE_API_KEY" environmental variable.
#'   \item \code{"cascade"} : Attempts to use one geocoder service and then uses
#'     a second geocoder service if the first service didn't return results.
#'     The services and order is specified by the cascade_order argument. 
#'     Note that this is not compatible with \code{full_results = TRUE} as geocoder
#'     services have different columns that they return.
#' }
#' @param cascade_order a vector with two character values for the method argument 
#'  in the order in which the geocoder services will be attempted for method = "cascade"
#'  (ie. \code{c('census', 'geocodio')})
#' @param lat latitude column name. Can be quoted or unquoted (ie. lat or 'lat').
#' @param long longitude column name. Can be quoted or unquoted (ie. long or 'long').
#' @param limit number of results to return per address. Note that not all methods support
#'  setting limit to a value other than 1. Also limit > 1 is not compatible 
#'  with batch geocoding if return_addresses = TRUE.
#' @param min_time minimum amount of time for a query to take (in seconds) if using
#'  Location IQ or OSM. This parameter is used to abide by API usage limits. You can
#'  set it to a lower value (ie. 0) if using a local Nominatim server, for instance.
#' @param api_url custom API URL. If specified, the default API URL will be overridden.
#'  This parameter can be used to specify a local Nominatim server.
#' @param timeout query timeout (in minutes)
#' 
#' @param mode set to 'batch' to force batch geocoding or 'single' to 
#'  force single address geocoding (one address per query). If not 
#'  specified then batch geocoding will be used if available
#'  (given method selected) when multiple addresses are provided, otherwise
#'  single address geocoding will be used.
#' @param full_results returns all data from the geocoder service if TRUE. 
#' If FALSE then only longitude and latitude are returned from the geocoder service.
#' @param unique_only only return results for unique addresses if TRUE
#' @param return_addresses return input addresses with results if TRUE. Note that
#'    most services return the input addresses with full_results = TRUE and setting
#'    return_addresses to FALSE does not prevent this.
#' 
#' @param flatten if TRUE then any nested dataframes in results are flattened if possible.
#'    Note that Geocodio batch geocoding results are flattened regardless.
#' @param batch_limit limit to the number of addresses in a batch geocoding query.
#'  Both geocodio and census batch geocoders have a 10,000 address limit so this
#'  is the default.
#' @param verbose if TRUE then detailed logs are output to the console
#' @param no_query if TRUE then no queries are sent to the geocoder and verbose is set to TRUE

#' @param custom_query API-specific parameters to be used, passed as a named list 
#'  (ie. \code{list(vintage = 'Current_Census2010')}).
#' @param return_type only used when method = 'census'. Two possible values: 
#' \itemize{
#'     \item \code{"locations"} (default)
#'     \item \code{"geographies"}: returns additional geography columns. 
#'     See the Census geocoder API documentation for more details.
#' }
#' @param iq_region 'us' (default) or 'eu'. Used for establishing API URL for the 'iq' method
#' @param geocodio_v version of geocodio api. 1.6 is default. Used for establishing API URL
#'   for the 'geocodio' method.
#' @param param_error if TRUE then an error will be thrown if certain parameters are invalid for the selected geocoder
#'   service (method). The parameters checked are limit, address, street, city, county, state, postalcode, and country.
#'   If method = 'cascade' then no errors will be thrown.
#' 
#' @return parsed geocoding results in tibble format
#' @examples
#' \donttest{
#' geo(street = "600 Peachtree Street NE", city = "Atlanta",
#'  state = "Georgia", method = "census")
#' 
#' geo(address = c("Tokyo, Japan", "Lima, Peru", "Nairobi, Kenya"),
#'  method = 'osm')
#' 
#' geo(county = 'Jefferson', state = "Kentucky", country = "US",
#'      method = 'osm')
#' }
#' @seealso \code{\link{geocode}} \code{\link{api_parameter_reference}}
#' @export
geo <- function(address = NULL, 
    street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL,
    method = 'census', cascade_order = c('census', 'osm'), lat = lat, long = long, limit = 1, 
    min_time = NULL, api_url = NULL, timeout = 20,
    mode = '', full_results = FALSE, unique_only = FALSE, return_addresses = TRUE, 
    flatten = TRUE, batch_limit = 10000, verbose = FALSE, no_query = FALSE, 
    custom_query = list(), return_type = 'locations', iq_region = 'us', geocodio_v = 1.6, 
    param_error = TRUE) {

  # NSE - Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  
  # Reference Variables ------------------------------------------------------------
  
  # All legal methods (besides 'cascade')
  method_services <- unique(tidygeocoder::api_parameter_reference[['method']])
  # which methods require an api key
  methods_requiring_api_key <- unique(tidygeocoder::api_parameter_reference[which(tidygeocoder::api_parameter_reference[['generic_name']] == 'api_key'), ][['method']])
  
  # Check parameter arguments --------------------------------------------------------
  # all legal method argument excluding 'cascade'

  # Check argument inputs
  stopifnot(is.logical(verbose), is.logical(no_query), is.logical(flatten), is.logical(param_error),
            is.logical(full_results), is.logical(unique_only), is.logical(return_addresses),
            is.numeric(limit), is.numeric(batch_limit), is.numeric(timeout),
            limit >= 1, batch_limit >= 1, timeout >= 0, is.list(custom_query))
  
  if (!(method %in% c('cascade', method_services))) {
    stop('Invalid method argument. See ?geo')
  } 
  
  if (!(cascade_order[1] %in% method_services) | !(cascade_order[2] %in% method_services) | (length(cascade_order) != 2) |  !(is.character(cascade_order))) {
    stop('Invalid cascade_order argument. See ?geo')
  }
  
  if (!(mode %in% c('', 'single', 'batch'))) {
    stop('Invalid mode argument. See ?geo')
  }
  
  if (!(return_type %in% c('geographies', 'locations'))) {
    stop('Invalid return_type argument. See ?geo')
  }
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  ## Address parsing and deduplication -------------------------------------------------------
  address_pack <- package_addresses(address, street, city, county, 
           state, postalcode, country)
  
  # count number of unique addresses
  num_unique_addresses <- nrow(address_pack$unique) # unique addresses
  # determine how many rows will be returned (either unique or includes duplicate address rows)
  # num_rows_to_return <- ifelse(unique_only, num_unique_addresses, nrow(address_pack$crosswalk))
  #num_rows_to_return <- num_unique_addresses # used for na values
  
  NA_value <- get_na_value(lat, long, rows = num_unique_addresses) # filler result to return if needed
  
  if (verbose == TRUE) message(paste0('Number of Unique Addresses: ', num_unique_addresses))
  
  # If no valid/non-blank addresses are passed then return NA
  if (num_unique_addresses == 1 & all(is.na(address_pack$unique))) {
    if (verbose == TRUE) message(paste0('No non-blank non-NA addreses found. Returning NA results.'))
    return(unpackage_addresses(address_pack, NA_value, unique_only, return_addresses))
  }
  
  ### Parameter Check ------------------------------------------------------
  # check if limit and address parameters that are used are valid for the method
  if (method == 'cascade') {
    # for cascade, only mark a parameter as illegal if it is illegal for both methods
    legal_parameters <- unique(c(get_generic_parameters(cascade_order[1]), 
                               get_generic_parameters(cascade_order[2])))
    
    # string value to show the user the selected methods for cascade
    str_cascade_meth <- paste0(' ("', paste0(cascade_order, collapse = '", "'), '")')
    
  } else {
    legal_parameters <- get_generic_parameters(method)
    str_cascade_meth <- '' # blank if method != 'cascade'
  }
  
  # add illegal parameters that are selected to this vector
  illegal_params <- c()
  
  # If a parameter is used that is not supported by the method then throw an error
  for (param in colnames(address_pack$unique)) {
    if (!(param %in% legal_parameters)) {
      illegal_params <- c(illegal_params, param)
    }
  }
  
  if (length(illegal_params) > 0) {
    param_message <- paste0('The following parameter(s) are not supported for the "', 
                           method,'"', str_cascade_meth,
                           ' method:\n\n', paste0(illegal_params, collapse = ' '),
                           '\n\nSee ?api_parameter_reference for more details.')
    
    if (param_error == TRUE) stop(param_message)
    else if (verbose == TRUE) message(param_message)
  }
  
  # Cascade method -------------------------------------------------------------
  # If method = 'cascade' is called then pass all function arguments 
  # except for method and param_error to geo_cascade() and return the results 
  if (method == 'cascade') {
    if (full_results == TRUE) stop("full_results = TRUE cannot be used with the cascade method.")
    if (limit != 1) stop("limit argument must be 1 (default) to use the cascade method.")
    
    return(do.call(geo_cascade, 
                   c(all_args[!names(all_args) %in% c('method', 'param_error')], list(param_error = FALSE))))
  }
  
  # determine if an illegal limit parameter value was used (ie. if limit !=1
  # then the method API must have a limit parameter)
  if ((limit != 1) & (!('limit' %in% legal_parameters))) {
    illegal_limit_message <- paste0('The limit parameter must be set to 1 (the default) because the "',  method,'" ',
                                    'method API service does not support a limit argument.\n\n',
                                    'See ?api_parameter_reference for more details.')
    
    if (param_error == TRUE) stop(illegal_limit_message)
    else if (verbose == TRUE) message(illegal_limit_message)
  }
  
  # Single Address geocoding -------------------------------------------------------------
  # If there are multiple addresses and we are using a method without a batch geocoder function 
  # OR the user has explicitly specified single address geocoding then call the 
  # single address geocoder in a loop (ie. recursively call this function)
  if ((num_unique_addresses > 1) & ((!(method %in% names(batch_func_map))) | (mode == 'single'))) {
      if (verbose == TRUE) {
        message('Executing single address geocoding...\n')
      }
      
      # construct arguments for a single address query
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
      
  # Batch geocoding --------------------------------------------------------------------------
  if ((num_unique_addresses > 1) | (mode == 'batch')) {
    
    if (limit != 1 & return_addresses == TRUE) {
    stop('For batch geocoding (more than one address per query) the limit argument must 
    be 1 (the default) OR the return_addresses argument must be FALSE. Possible solutions:
    1) Set the mode argument to "single" to force single (not batch) geocoding 
    2) Set limit argument to 1 (ie. 1 result is returned per address)
    3) Set return_addresses to FALSE
    See the geo() function documentation for details.')
    }
  
    # Enforce batch limit if needed
    if (num_unique_addresses > batch_limit) {
      message(paste0('Limiting batch query to ', format(batch_limit, big.mark = ','), ' addresses'))
      batch_addresses <-  address_pack$unique[1:batch_limit, ]
    } else {
      batch_addresses <- address_pack$unique
    }
    
    if (verbose == TRUE) message(paste0('Passing ', 
      format(min(batch_limit, num_unique_addresses), big.mark = ','), 
                          ' addresses to the ', method, ' batch geocoder'))
    
    # Convert our generic query parameters into parameters specific to our API (method)
    if (no_query == TRUE) return(unpackage_addresses(address_pack, 
         get_na_value(lat, long, rows = num_unique_addresses), 
         unique_only, return_addresses))
    
    # call the appropriate function for batch geocoding according the the batch_func_map named list
    # if batch limit was exceeded then apply that limit
    batch_results <- do.call(batch_func_map[[method]], c(list(batch_addresses),
          all_args[!names(all_args) %in% pkg.globals$address_arg_names]))
    
    # Add NA results if batch limit was reached so rows match up
    if (num_unique_addresses > batch_limit) {
      batch_filler <- get_na_value(lat, long, rows = num_unique_addresses - batch_limit)
      batch_results <- dplyr::bind_rows(batch_results, batch_filler)
    }
    
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
  
  # Start to build 'generic' query as named list -------------------------
  generic_query <- list()
  
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
  
  # Set API URL (if not already set) ---------------------------
  if (is.null(api_url)) {
    api_url <- switch(method,
                      "census" = get_census_url(return_type, search),
                      "osm" = get_osm_url(),
                      "geocodio" = get_geocodio_url(geocodio_v),
                      "iq" = get_iq_url(iq_region),
                      "google" = get_google_url()
    )
  }
  if (length(api_url) == 0) stop('API URL not found')
  
  # Set min_time if not set based on usage limit of service
  if (is.null(min_time)) {
    if (method %in% names(usage_limit_map)) min_time <- usage_limit_map[[method]]
    else min_time <- 0 # default to 0
  }

  # If API key is required then use the get_key() function to retrieve it
  if (method %in% methods_requiring_api_key) {
    generic_query[['api_key']] <- get_key(method)
  }
  
  if (!is.null(limit)) generic_query[['limit']] <- limit
  
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method, generic_query, custom_query)
  
  # Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  if (no_query == TRUE) return(unpackage_addresses(address_pack, NA_value, unique_only, return_addresses))
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  
  ## Extract results -----------------------------------------------------------------------------------
  # output error message for geocodio if present
  if ((method == 'geocodio') & (!is.data.frame(raw_results)) & ("error" %in% names(raw_results))) {
    message(paste0('Error: ', raw_results$error))
    results <- NA_value
  } 
  # output error message for google if present
  else if ((method == 'google') & (!is.data.frame(raw_results)) & ("error_message" %in% names(raw_results))) {
    message(paste0('Error: ', raw_results$error_message))
    results <- NA_value
  } 
  else if (length(raw_results) == 0) {
    # If no results found, return NA
    # otherwise extract results
    results <- NA_value
    if (verbose == TRUE) message("No results found")
  } 
  else {
    # Extract results. Use the full_results and flatten parameters
    # to control the output
    results <- extract_results(method, raw_results, full_results, flatten)
    
    # Name the latitude and longitude columns in accordance with lat/long arguments
    names(results)[1] <- lat
    names(results)[2] <- long
  }
  
  # Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  if (verbose == TRUE) message() # insert ending line break if verbose
  
  return(unpackage_addresses(address_pack, results, unique_only, return_addresses))
}
