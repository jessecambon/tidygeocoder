## References
# google: https://developers.google.com/maps/documentation/geocoding/start
# geocodio: https://www.geocod.io/docs/#reverse-geocoding
# osm: https://nominatim.org/release-docs/latest/api/Reverse/
# opencage: https://opencagedata.com/api

## NOTE: geocodio supports BATCH reverse geocoding

## IMPORTANT: All new REVERSE batch geocoding functions must be added to reverse_batch_func_map
# the reverse_geo() function references this list to find reverse batch geocoding functions (reverse_batch_geocoding.R)
# maps method names to batch functions
reverse_batch_func_map <- list(
  geocodio = reverse_batch_geocodio
)


# Create API parameters for a single set of coordinates (lat, long) based on the 
# method. Parameters are placed into the 'custom_query' variable which is a named list
# that is passed directly to the API service.
get_coord_parameters <- function(custom_query, method, lat, long) {
  if (method %in% c('osm', 'iq')) {
    custom_query[['lat']] <- lat
    custom_query[['lon']] <- long
  } else if (method %in% c('geocodio', 'opencage')) {
    custom_query[['q']] <-  paste0(as.character(lat), ',', as.character(long))
  } else if (method == 'google') {
    custom_query[['latlng']] <-  paste0(as.character(lat), ',', as.character(long))
  } else {
    stop('Invalid method.')
  }
  return(custom_query)
}








#' Reverse geocode latitude, longitude coordinates
#' 
#' @description
#' Reverse geocodes coordinates given as numeric values. The \code{\link{reverse_geocode}}
#' function utilizes this function on coordinates contained in dataframes.
#' See example usage in \code{vignette("tidygeocoder")} 
#'
#' This function uses the \code{\link{get_api_query}}, \code{\link{query_api}}, and
#' \code{\link{reverse_extract_results}} functions to create, execute, and parse the geocoder
#' API queries.
#' 
#' @param lat latitude
#' @param long longitude
#' 
#' @param method the geocoder service to be used. Refer to 
#' \code{\link{api_parameter_reference}} and the API documentation for
#' each geocoder service for usage details and limitations.
#' \itemize{
#'   \item \code{"osm"}: Nominatim (OSM). Worldwide coverage.
#'   \item \code{"geocodio"}: Commercial geocoder. Covers US and Canada and has
#'      batch geocoding capabilities. Requires an API Key to be stored in
#'      the "GEOCODIO_API_KEY" environmental variable.
#'   \item \code{"iq"}: Commercial Nominatim geocoder service. Requires an API Key to
#'      be stored in the "LOCATIONIQ_API_KEY" environmental variable.
#'   \item \code{"google"}: Commercial Google geocoder service. Requires an API Key to
#'      be stored in the "GOOGLEGEOCODE_API_KEY" environmental variable.
#'   \item \code{"opencage"}: Commercial geocoder with
#'      \href{https://opencagedata.com/credits}{various open data sources} (e.g.
#'      OpenStreetMap) and worldwide coverage. Requires an API Key to be stored
#'      in the "OPENCAGE_KEY" environmental variable.
#' }
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
#' @param return_coords return input coordinates with results if TRUE. Note that
#'    most services return the input coordinates with full_results = TRUE and setting
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
#' @param iq_region 'us' (default) or 'eu'. Used for establishing API URL for the 'iq' method
#' @param geocodio_v version of geocodio api. 1.6 is default. Used for establishing API URL
#'   for the 'geocodio' method.
#' 
#' @return parsed geocoding results in tibble format
#' @examples
#' \donttest{
#'  reverse_geo(lat = 38.895865, long = -77.0307713, method = 'osm', verbose = TRUE)
#'  
#'  reverse_geo(lat = c(38.895865, 43.6534817, 300), 
#'  long = c(-77.0307713, -79.3839347, 600), method = 'osm', full_results = TRUE, verbose = TRUE)
#'  
#' }
#' @seealso \code{\link{geocode}} \code{\link{api_parameter_reference}}
#' @export
reverse_geo <- function(lat, long, method = 'osm', address = address, limit = 1, min_time = NULL, api_url = NULL,  
    timeout = 20, mode = '',  full_results = FALSE, unique_only = FALSE, return_coords = TRUE, flatten = TRUE, 
    batch_limit = 10000, verbose = FALSE, no_query = FALSE, custom_query = list(), iq_region = 'us', geocodio_v = 1.6) {

  # NSE eval
  address <- rm_quote(deparse(substitute(address)))
  
  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  
  # Reference Variables ------------------------------------------------------------

  # Check argument inputs
  stopifnot(is.logical(verbose), is.logical(no_query), is.logical(flatten),
      is.logical(full_results), is.logical(unique_only),
      is.numeric(limit), limit >= 1,  is.list(custom_query))
  
  if (length(lat) != length(long)) stop('Lengths of lat and long must be equal.')
  
  coord_pack <- package_inputs(tibble::tibble(lat = as.numeric(lat), long = as.numeric(long)), coords = TRUE)
  num_coords <- nrow(coord_pack$unique)
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  # Geocode coordinates one at a time in a loop -------------------------------------------------------
  if ((num_coords > 1) & ((!(method %in% names(reverse_batch_func_map))) | (mode == 'single'))) {
    # construct arguments for a single address query
    # note that non-lat/long related fields go to the MoreArgs argument of mapply
    # since we aren't iterating through them
    single_coord_args <- c(
      list(FUN = reverse_geo, lat = coord_pack$unique$lat, long = coord_pack$unique$long),
      list(MoreArgs = all_args[!names(all_args) %in% c('lat', 'long')],
           USE.NAMES = FALSE, SIMPLIFY = FALSE)
    )
    
    # Reverse geocode each coordinate individually by recalling this function with mapply
    list_coords <- do.call(mapply, single_coord_args)
    # rbind the list of tibble dataframes together
    stacked_results <- dplyr::bind_rows(list_coords)
    
    # note that return_inputs has been set to FALSE here since lat/long coordinates will already
    # be returned in the first geo function call (if asked for)
    return(unpackage_inputs(coord_pack, stacked_results, unique_only, FALSE))
  }
  
  # Batch geocoding --------------------------------------------------------------------------
  if ((num_coords > 1) | (mode == 'batch')) {
    
    if (verbose == TRUE) message(paste0('Passing ', 
            format(min(batch_limit, num_coords), big.mark = ','), 
            ' coordinates to the ', method, ' batch geocoder'))
    
    # call the appropriate function for batch geocoding according the the batch_func_map named list
    # if batch limit was exceeded then apply that limit
    batch_results <- do.call(reverse_batch_func_map[[method]], 
        c(list(lat = coord_pack$unique$lat, long = coord_pack$unique$long),
        all_args[!names(all_args) %in% c('lat', 'long')]))
    
    # map the raw results back to the original lat,long inputs that were passed if there are duplicates
    return(unpackage_inputs(coord_pack, batch_results, unique_only, return_coords))
  }
  

  ################################################################################
  #### Code past this point is for reverse geocoding a single coordinate set #####
  ################################################################################
  
  # Start to build 'generic' query as named list -----------------------------
  generic_query <- list()
  
  # Create parameters for lat, long coordinates
  custom_query <- get_coord_parameters(custom_query, method, lat, long)
  
  # Set API URL (if not already set) ----------------------------------------
  if (is.null(api_url)) {
    api_url <- get_api_url(method, reverse = TRUE, geocodio_v = geocodio_v, iq_region = iq_region)
  }
  if (length(api_url) == 0) stop('API URL not found')
  
  # Set min_time if not set based on usage limit of service
  if (is.null(min_time)) min_time <- get_min_query_time(method)
  
  if (!is.null(limit)) generic_query[['limit']] <- limit
  
  # If API key is required then use the get_key() function to retrieve it
  if (method %in% get_services_requiring_key()) {
    generic_query[['api_key']] <- get_key(method)
  }
  
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method, generic_query, custom_query)
  
  # Execute Single Coordinate Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  
  ## Extract results ------------------------------------------------------------------------------
  if (length(raw_results) == 0) {
    # If no results found, return NA
    # otherwise extract results
    results <- tibble::tibble(address = as.character(NA))
    if (verbose == TRUE) message("No results found")
  } 
  else {
    results <- extract_reverse_results(method, raw_results, full_results, flatten)
  }
  
  # rename address column
  names(results)[1] <- address

  # Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  if (verbose == TRUE) message() # insert ending line break if verbose

  return(unpackage_inputs(coord_pack, results, unique_only, return_coords))
}
  
### Test queries
# a <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'osm', verbose = TRUE)
# b <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'google', full_results = TRUE, verbose = TRUE)

# c <- reverse_geo(lat = c(38.895865, 43.6534817, 300), long = c(-77.0307713, -79.3839347, 600), method = 'geocodio', full_results = TRUE, verbose = TRUE)

# bq1 <- reverse_geocode(tibble::tibble(latitude = c(38.895865, 43.6534817, 700), longitude = c(-77.0307713, -79.3839347, 300)), 
# lat = latitude, long = longitude, method = 'osm', full_results = TRUE, verbose = TRUE)