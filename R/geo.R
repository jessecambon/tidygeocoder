
## IMPORTANT: All new batch geocoding functions must be added to batch_func_map
# the geo() function references this list to find batch geocoding functions (batch_geocoding.R)
# maps method names to batch functions
batch_func_map <- list(
  geocodio = batch_geocodio, 
  census = batch_census,
  here = batch_here,
  tomtom = batch_tomtom,
  mapquest = batch_mapquest,
  bing = batch_bing
)

#' Geocode addresses
#' 
#' @description
#' Geocodes addresses given as character values. The [geocode]
#' function utilizes this function on addresses contained in dataframes.
#' See example usage in `vignette("tidygeocoder")`.
#' 
#' Note that not all geocoder services support certain address component 
#' parameters. For example, the Census geocoder only covers the United States 
#' and does not have a "country" parameter. 
#' 
#' Refer to [api_parameter_reference],
#' [min_time_reference], and [batch_limit_reference] for more details on 
#' geocoder service parameters and usage. 
#' 
#' This function uses the [get_api_query], [query_api], and
#' [extract_results] functions to create, execute, and parse geocoder
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
#' @param method `r get_method_documentation(reverse = FALSE)`
#'  - `"cascade"` : First uses one geocoder service and then uses
#'     a second geocoder service if the first service didn't return results.
#'     The services and order is specified by the cascade_order argument. 
#'     Note that this is not compatible with `full_results = TRUE` as geocoder
#'     services have different columns that they return.
#' 
#' @param cascade_order a vector with two character values for the method argument 
#'  in the order in which the geocoder services will be attempted for `method = "cascade"`
#'  (ie. `c("census", "geocodio")`)
#' @param lat latitude column name. Can be quoted or unquoted (ie. lat or "lat").
#' @param long longitude column name. Can be quoted or unquoted (ie. long or "long").
#' @param limit `r get_limit_documentation(reverse = FALSE, df_input = FALSE)`
#' @param min_time minimum amount of time for a query to take (in seconds). If NULL
#' then min_time will be set to the default value specified in [min_time_reference].
#' @param api_url custom API URL. If specified, the default API URL will be overridden.
#'  This parameter can be used to specify a local Nominatim server, for instance.
#' @param timeout query timeout (in minutes)
#' 
#' @param mode `r get_mode_documentation(reverse = FALSE)`

#' @param full_results returns all data from the geocoder service if TRUE. 
#' If FALSE then only longitude and latitude are returned from the geocoder service.
#' @param unique_only only return results for unique inputs if TRUE
#' @param return_addresses return input addresses with results if TRUE. Note that
#'    most services return the input addresses with `full_results = TRUE` and setting
#'    return_addresses to FALSE does not prevent this.
#' 
#' @param flatten if TRUE then any nested dataframes in results are flattened if possible.
#'    Note that in some cases results are flattened regardless such as for
#'    Geocodio batch geocoding.
#' @param batch_limit  `r get_batch_limit_documentation(reverse = FALSE)`
#' @param batch_limit_error `r get_batch_limit_error_documentation(reverse = FALSE)`
#' @param verbose if TRUE then detailed logs are output to the console
#' @param no_query if TRUE then no queries are sent to the geocoder service and verbose is set to TRUE.
#'    Used for testing.

#' @param custom_query API-specific parameters to be used, passed as a named list 
#'  (ie. `list(extratags = 1)`.
#' @param return_type only used when `method = "census"`. Two possible values: 
#'   - `"locations"` (default)
#'   - `"geographies"`: returns additional geography columns. 
#'   See the Census geocoder API documentation for more details.
#' @param iq_region "us" (default) or "eu". Used for establishing the API URL for the "iq" method.
#' @param geocodio_v version of geocodio API. Used for establishing the API URL
#'   for the "geocodio" method.
#' @param param_error if TRUE then an error will be thrown if any address parameters are used that are
#'   invalid for the selected service (`method`). If `method = "cascade"` then no errors will be thrown.
#' @param mapbox_permanent if TRUE then the `mapbox.places-permanent`
#'   endpoint would be used. Note that this option should be used only if you 
#'   have applied for a permanent account. Unsuccessful requests made by an 
#'   account that does not have access to the endpoint may be billable.
#' @param here_request_id This parameter would return a previous HERE batch job,
#'   identified by its RequestID. The RequestID of a batch job is displayed 
#'   when `verbose` is TRUE. Note that this option would ignore the 
#'   current `address` parameter on the request, so `return_addresses`
#'   needs to be FALSE.
#' @param mapquest_open if TRUE then MapQuest would use the Open Geocoding 
#'   endpoint, that relies solely on data contributed to OpenStreetMap.
#'    
#' @return tibble (dataframe)
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
#' @seealso [geocode] [api_parameter_reference] [min_time_reference] [batch_limit_reference]
#' @export
geo <- function(address = NULL, 
    street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL,
    method = 'osm', cascade_order = c('census', 'osm'), lat = lat, long = long, limit = 1, 
    min_time = NULL, api_url = NULL, timeout = 20,
    mode = '', full_results = FALSE, unique_only = FALSE, return_addresses = TRUE, 
    flatten = TRUE, batch_limit = NULL, batch_limit_error = TRUE, verbose = FALSE, no_query = FALSE, 
    custom_query = list(), return_type = 'locations', iq_region = 'us', geocodio_v = 1.6, 
    param_error = TRUE, mapbox_permanent = FALSE, here_request_id = NULL,
    mapquest_open = FALSE) {

  # NSE - Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  
  # All legal methods (besides 'cascade')
  method_services <- unique(tidygeocoder::api_parameter_reference[['method']])

  # Check parameter arguments --------------------------------------------------------

  # Check argument inputs
  check_address_argument_datatype(address, 'address')
  check_address_argument_datatype(street, 'street')
  check_address_argument_datatype(city, 'city')
  check_address_argument_datatype(county, 'county')
  check_address_argument_datatype(state, 'state')
  check_address_argument_datatype(postalcode, 'postalcode')
  check_address_argument_datatype(country, 'country')
  
  stopifnot(
    is.logical(verbose), is.logical(no_query), is.logical(flatten), is.logical(param_error),
            is.logical(full_results), is.logical(unique_only), is.logical(return_addresses),
            is.logical(batch_limit_error), 
            is.numeric(timeout), timeout >= 0, 
            is.list(custom_query),
            is.logical(mapbox_permanent), 
            is.null(here_request_id) || is.character(here_request_id),
            is.logical(mapquest_open)
    )
  
  check_common_args('geo', mode, limit, batch_limit, min_time)
  
  if (mode == 'batch' && (!method %in% names(batch_func_map))) {
    stop(paste0('The "', method, '" method does not have a batch geocoding function. See ?geo') , call. = FALSE)
  }
  
  if (!(method %in% c('cascade', method_services))) {
    stop('Invalid method argument. See ?geo', call. = FALSE)
  } 
  
  if (!(cascade_order[1] %in% method_services) || !(cascade_order[2] %in% method_services) || (length(cascade_order) != 2) || !(is.character(cascade_order))) {
    stop('Invalid cascade_order argument. See ?geo', call. = FALSE)
  }
  
  if (method == 'cascade' && mode == 'batch' && (length(intersect(cascade_order, names(batch_func_map)) != 2))) {
    stop("To use method = 'cascade' and mode = 'batch', both methods specified in cascade_order
          must have batch geocoding capabilities. See ?geo")
  }
  
  if (!(return_type %in% c('geographies', 'locations'))) {
    stop('Invalid return_type argument. See ?geo', call. = FALSE)
  }
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  ## Address parsing and deduplication -------------------------------------------------------
  address_pack <- package_addresses(address, street, city, county, 
           state, postalcode, country)
  
  # count number of unique addresses
  num_unique_addresses <- nrow(address_pack$unique) # unique addresses
  NA_value <- get_na_value(lat, long, rows = num_unique_addresses) # filler result to return if needed
  
  if (verbose == TRUE) message(paste0('Number of Unique Addresses: ', num_unique_addresses))
  
  # If no valid/non-blank addresses are passed then return NA
  if (num_unique_addresses == 1 && all(is.na(address_pack$unique))) {
    if (verbose == TRUE) message(paste0('No non-blank non-NA addreses found. Returning NA results.'))
    return(unpackage_inputs(address_pack, NA_value, unique_only, return_addresses))
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
    
    if (param_error == TRUE) stop(param_message, call. = FALSE)
    else if (verbose == TRUE) message(param_message)
  }
  
  # Cascade method -------------------------------------------------------------
  # If method = 'cascade' is called then pass all function arguments 
  # except for method, param_error, and batch_limit_error
  # param_error and batch_limit_error are reverted to FALSE
  if (method == 'cascade') {
    if (full_results == TRUE) stop("full_results = TRUE cannot be used with the cascade method.", call. = FALSE)
    if (is.null(limit) || limit != 1) stop("limit argument must be 1 (default) to use the cascade method.", call. = FALSE)
    
    return(do.call(cascade_geocoding, 
                c(all_args[!names(all_args) %in% c('method', 'param_error', 'batch_limit_error')],
                list(batch_limit_error = FALSE, param_error = FALSE))))
  }
  
  # Single Address geocoding -------------------------------------------------------------
  # If there are multiple addresses and we are using a method without a batch geocoder function 
  # OR the user has explicitly specified single address geocoding then call the 
  # single address geocoder in a loop (ie. recursively call this function)
  
  # Exception for geocoder services that should default to single instead of batch
  if (method %in% pkg.globals$single_first_methods && mode != 'batch' ){
    mode <- 'single'
  }
  
  # Single address geocoding is used if the method has no batch function or if 
  # mode = 'single' was specified
  if ((num_unique_addresses > 1) && ((!(method %in% names(batch_func_map))) || (mode == 'single'))) {
      if (verbose == TRUE) message('Executing single address geocoding...\n')
      
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
      
      # note that return_inputs has been set to FALSE here since addresses will already
      # be returned in the first geo function call (if asked for)
      return(unpackage_inputs(address_pack, stacked_results, unique_only, FALSE))
      }
      
  # Batch geocoding --------------------------------------------------------------------------
  if ((num_unique_addresses > 1) || (mode == 'batch')) {
    if (verbose == TRUE) message('Executing batch geocoding...\n')
    
    if ((is.null(limit) || limit != 1) && return_addresses == TRUE) {
    stop('For batch geocoding (more than one address per query) the limit argument must 
    be 1 (the default) OR the return_addresses argument must be FALSE. Possible solutions:
    1) Set the mode argument to "single" to force single (not batch) geocoding 
    2) Set limit argument to 1 (ie. 1 result is returned per address)
    3) Set return_addresses to FALSE
    See the geo() function documentation for details.', call. = FALSE)
    }
    
    # set batch limit to default if not specified
    if (is.null(batch_limit)) batch_limit <- get_batch_limit(method)
    if (verbose == TRUE) message(paste0('Batch limit: ', 
                                        format(batch_limit, big.mark = ',')))
    
    # Enforce batch limit if needed
    if (num_unique_addresses > batch_limit) {
      batch_limit_exceeded <- TRUE
      batch_limit_exceeded_message <- paste0(format(num_unique_addresses, big.mark = ','),
          ' unique addresses found which exceeds the batch limit of ', 
                                             format(batch_limit, big.mark = ','), '.')
      
      if (batch_limit_error == TRUE) stop(batch_limit_exceeded_message, call. = FALSE)
      else {
        warning(paste0(batch_limit_exceeded_message, '\n', 
        'Only geocoding the first ', format(batch_limit, big.mark = ','), ' unique addresses. ',
                       'All other addresses will have NA results.'))
        
        # apply batch limit to query
        batch_unique_addresses <- address_pack$unique[1 : batch_limit, ]
      }
    } else {
      # if batch limit wasn't exceeded we just will use the unique addresses in address_pack
      batch_limit_exceeded <- FALSE
      batch_unique_addresses <- address_pack$unique
    }
    
    # HERE: If a previous job is requested return_addresses should be FALSE
    # This is because the job won't send the addresses, but would recover the
    # results of a previous request
    if (method == 'here' && is.character(here_request_id) && return_addresses == TRUE) {
      stop('HERE: When requesting a previous job via here_request_id, set return_addresses to FALSE.
      See the geo() function documentation for details.', call. = FALSE)
      }

    if (verbose == TRUE) message(paste0('Passing ', 
      format(min(batch_limit, num_unique_addresses), big.mark = ','), 
                          ' addresses to the ', method, ' batch geocoder'))
    
    # return NA results if no_query == TRUE
    if (no_query == TRUE) return(unpackage_inputs(address_pack, NA_value, unique_only, return_addresses))
    
    # call the appropriate function for batch geocoding according the the batch_func_map named list
    # if batch limit was exceeded then apply that limit
    batch_results <- do.call(batch_func_map[[method]], c(list(batch_unique_addresses),
          all_args[!names(all_args) %in% pkg.globals$address_arg_names]))
    
    # If batch limit was exceeded we need to add some NA results to make our results
    # line up with the input addresses
    if (batch_limit_exceeded == TRUE) {
      batch_filler <- get_na_value(lat, long, rows = num_unique_addresses - batch_limit)
      batch_results <- dplyr::bind_rows(batch_results, batch_filler)
    }
    
    # if verbose = TRUE, tell user how long batch query took
    if (verbose == TRUE) {
      batch_time_elapsed <- get_seconds_elapsed(start_time)
      print_time("Query completed in", batch_time_elapsed)
      message('') # line break
    }
    
    # map the raw results back to the original addresses that were passed if there are duplicates
    return(unpackage_inputs(address_pack, batch_results, unique_only, return_addresses))
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
    api_url <- get_api_url(method, reverse = FALSE, return_type = return_type,
                search = search, geocodio_v = geocodio_v, iq_region = iq_region, 
                mapbox_permanent = mapbox_permanent, mapquest_open = mapquest_open)
  }
  
  # Workaround for Mapbox/TomTom - The search_text should be in the API URL
  if (method %in% c('mapbox', 'tomtom')) {
    api_url <- gsub(" ", "%20", paste0(api_url, generic_query[['address']], ".json"))
    # Remove semicolons (Reserved for batch)
    api_url <- gsub(";", ",", api_url)
  }

  # Set min_time if not set based on usage limit of service
  if (is.null(min_time)) min_time <- get_min_query_time(method)

  # add limit and api_key to generic query
  generic_query <- add_common_generic_parameters(generic_query, method, no_query, limit)
  
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method, generic_query, custom_query)
  
  # Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  
  # return NA results if no_query = TRUE
  if (no_query == TRUE) return(unpackage_inputs(address_pack, NA_value, unique_only, return_addresses))
  query_results <- query_api(api_url, api_query_parameters, method = method)
  
  if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(query_results$status)))
  
  ## Extract results -----------------------------------------------------------------------------------
  # if there were problems with the results then return NA
  if (query_results$status != 200) {
    extract_errors_from_results(method, query_results$content, verbose)
    results <- NA_value
  }
  else {
    # Extract results. Use the full_results and flatten parameters
    # to control the output
    results <- extract_results(method, jsonlite::fromJSON(query_results$content), full_results, flatten, limit)
    
    # Name the latitude and longitude columns in accordance with lat/long arguments
    names(results)[1] <- lat
    names(results)[2] <- long
  }
  
  # Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  if (verbose == TRUE) message('') # insert ending line break if verbose
  
  return(unpackage_inputs(address_pack, results, unique_only, return_addresses))
}
