## References
# google: https://developers.google.com/maps/documentation/geocoding/start
# geocodio: https://www.geocod.io/docs/#reverse-geocoding
# osm: https://nominatim.org/release-docs/latest/api/Reverse/
# opencage: https://opencagedata.com/api

## IMPORTANT: All new REVERSE batch geocoding functions must be added to reverse_batch_func_map
# the reverse_geo() function references this list to find reverse batch geocoding functions (reverse_batch_geocoding.R)
# maps method names to batch functions
reverse_batch_func_map <- list(
  geocodio = reverse_batch_geocodio,
  here = reverse_batch_here,
  tomtom = reverse_batch_tomtom,
  mapquest = reverse_batch_mapquest,
  bing = reverse_batch_bing
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
  } else if (method == 'mapbox') {
    custom_query[['to_url']] <-
      paste0(as.character(long), ',', as.character(lat))
  } else if (method == 'here') {
    custom_query[['at']] <- 
      paste0(as.character(lat), ',', as.character(long))
  } else if (method == 'tomtom') {
    custom_query[['to_url']] <- 
      paste0(as.character(lat), ',', as.character(long))
  } else if (method == 'mapquest') {
    custom_query[['location']] <-  paste0(as.character(lat), ',', as.character(long)) 
  } else if (method == 'bing') {
    custom_query[['to_url']] <-  paste0('/', as.character(lat), ',', as.character(long)) 
  } else if (method == 'arcgis'){
    custom_query[['location']] <-
      paste0(as.character(long), ',', as.character(lat))
  } else {
    stop('Invalid method. See ?reverse_geo', call. = FALSE)
  }
  return(custom_query)
}


#' Reverse geocode coordinates
#' 
#' @description
#' Reverse geocodes geographic coordinates (latitude and longitude) given as numeric values. 
#' Latitude and longitude inputs are limited to possible values. Latitudes must be between -90 and 90 and
#' longitudes must be between -180 and 180. Invalid values will not be sent to the geocoder service. 
#' The \code{\link{reverse_geocode}} function utilizes this function on coordinates contained in dataframes.
#' See example usage in \code{vignette("tidygeocoder")}.
#'
#' This function uses the \code{\link{get_api_query}}, \code{\link{query_api}}, and
#' \code{\link{extract_reverse_results}} functions to create, execute, and parse the geocoder
#' API queries.
#' 
#' @param lat latitude values (input data)
#' @param long longitude values (input data)
#' @param method the geocoder service to be used. Refer to 
#' \code{\link{api_parameter_reference}} and the API documentation for
#' each geocoder service for usage details and limitations. Note that the 
#' Census service does not support reverse geocoding. Run \code{usethis::edit_r_environ()}
#' to open your .Renviron file for editing to add API keys as an environmental variables.
#' \itemize{
#'   \item \code{"osm"}: Nominatim (OSM) geocoder service.
#'   \item \code{"arcgis"}: Commercial ArcGIS geocoder service.
#'   \item \code{"geocodio"}: Commercial geocoder. Covers US and Canada and has
#'      batch geocoding capabilities. Requires an API Key to be stored in
#'      the "GEOCODIO_API_KEY" environmental variable.
#'   \item \code{"iq"}: Location IQ geocoder service. Requires an API Key to
#'      be stored in the "LOCATIONIQ_API_KEY" environmental variable.
#'   \item \code{"google"}: Google geocoder service. Requires an API Key to
#'      be stored in the "GOOGLEGEOCODE_API_KEY" environmental variable.
#'   \item \code{"opencage"}: Commercial Open Cage geocoder with
#'      \href{https://opencagedata.com/credits}{various open data sources} (e.g.
#'      OpenStreetMap). Requires an API Key to be stored
#'      in the "OPENCAGE_KEY" environmental variable.
#'   \item \code{"mapbox"}: Commercial Mapbox geocoder service. Requires an API Key to
#'      be stored in the "MAPBOX_API_KEY" environmental variable.
#'   \item \code{"here"}: Commercial HERE geocoder service. Requires an API Key 
#'      to be stored in the "HERE_API_KEY" environmental variable. Can perform 
#'      batch geocoding, but this must be specified with \code{mode = 'batch'}.
#'   \item \code{"tomtom"}: Commercial TomTom geocoder service. Requires an API Key to
#'      be stored in the "TOMTOM_API_KEY" environmental variable. Can perform
#'      batch geocoding.
#'   \item \code{"mapquest"}: Commercial MapQuest geocoder service. Requires an 
#'      API Key to be stored in the "MAPQUEST_API_KEY" environmental variable. 
#'      Can perform batch geocoding.
#'   \item \code{"bing"}: Commercial Bing geocoder service. Requires an 
#'      API Key to be stored in the "BINGMAPS_API_KEY" environmental variable. 
#'      Can perform batch geocoding.
#' }
#' 
#' @param address name of the address column (output data)
#' @param limit maximum number of results to return per coordinate For many geocoder services
#'   the maximum value for the limit parameter is 100. 
#'   Use `limit = NULL` to use the default value of the selected geocoder service. 
#'   For batch geocoding, limit must be set to 1 (default) if `return_coords = TRUE`.
#'   See See \code{\link{api_parameter_reference}} for more information.
#' Refer to API documentation of each service for more information.
#' @param min_time minimum amount of time for a query to take (in seconds). If NULL
#' then min_time will be set to the lowest value that complies with the usage requirements of 
#' the free tier of the selected geocoder service. See \code{\link{min_time_reference}} for
#' default values.
#' @param api_url custom API URL. If specified, the default API URL will be overridden.
#'  This parameter can be used to specify a local Nominatim server.
#' @param timeout query timeout (in minutes)
#' 
#' @param mode set to 'batch' to force batch geocoding or 'single' to 
#'  force single address geocoding (one coordinate per query). If not 
#'  specified then batch geocoding will be used if available
#'  (given method selected) when multiple coordinates are provided; otherwise
#'  single address geocoding will be used. For 'here' and 'bing' the batch mode
#'  should be explicitly specified with `mode = 'batch'`.
#' @param full_results returns all data from the geocoder service if TRUE. 
#' If FALSE then only a single address column will be returned from the geocoder service.
#' @param unique_only only return results for unique addresses if TRUE
#' @param return_coords return input coordinates with results if TRUE. Note that
#'    most services return the input coordinates with `full_results = TRUE` and setting
#'    return_addresses to FALSE does not prevent this.
#' @param flatten if TRUE then any nested dataframes in results are flattened if possible.
#'    Note that Geocodio batch geocoding results are flattened regardless.
#' @param batch_limit limit to the number of addresses in a batch geocoding query.
#'  Both geocodio and census batch geocoders have a 10,000 limit so this
#'  is the default. 'here' has a 1,000,000 address limit. 'mapquest' has a 100 address
#'  limit. 'bing' as a 50 address limit.
#' @param verbose if TRUE then detailed logs are output to the console
#' @param no_query if TRUE then no queries are sent to the geocoder and verbose is set to TRUE
#' @param custom_query API-specific parameters to be used, passed as a named list 
#'  (ie. `list(extratags = 1)`).
#' @param iq_region 'us' (default) or 'eu'. Used for establishing API URL for the 'iq' method
#' @param geocodio_v version of geocodio api. Used for establishing API URL
#'   for the 'geocodio' method.
#' @param mapbox_permanent if TRUE then the \code{mapbox.places-permanent} 
#'   endpoint would be used. Note that this option should be used only if you 
#'   have applied for a permanent account. Unsuccessful requests made by an 
#'   account that does not have access to the endpoint may be billable.
#' @param here_request_id This parameter would return a previous HERE batch job,
#'   identified by its RequestID. The RequestID of a batch job is displayed 
#'   when `verbose = TRUE`. Note that this option would ignore the 
#'   current \code{lat, long} parameters on the request, so \code{return_coords} 
#'   needs to be FALSE.
#' @param mapquest_open if TRUE then MapQuest would use the Open Geocoding 
#'   endpoint, that relies solely on data contributed to OpenStreetMap.
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
    batch_limit = NULL, verbose = FALSE, no_query = FALSE, custom_query = list(), iq_region = 'us', geocodio_v = 1.6,
    mapbox_permanent = FALSE, here_request_id = NULL, mapquest_open = FALSE) {

  # NSE eval
  address <- rm_quote(deparse(substitute(address)))
  
  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  
  # Check argument inputs
  stopifnot(is.logical(verbose), is.logical(no_query), is.logical(flatten),
      is.logical(full_results), is.logical(unique_only),
      is.list(custom_query), 
      is.logical(mapbox_permanent),
      is.null(here_request_id) || is.character(here_request_id),
      is.logical(mapquest_open)
  )
  
  # Check method argument
  # Currently census is the only method that doesn't support reverse geocoding
  method_services <- unique(tidygeocoder::api_parameter_reference[['method']])
  if (!(method %in% method_services[!method_services %in% c('census')])) {
    stop('Invalid method argument. See ?reverse_geo', call. = FALSE)
  } 
  
  if (length(lat) != length(long)) stop('Lengths of lat and long must be equal.', call. = FALSE)
  lat <- as.numeric(lat)
  long <- as.numeric(long)
  
  check_common_args('reverse_geo', mode, limit, batch_limit, min_time)
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  # Package inputs
  coord_pack <- package_inputs(tibble::tibble(lat = lat, long = long), coords = TRUE)
  num_unique_coords <- nrow(coord_pack$unique)
  if (verbose == TRUE) message(paste0('Number of Unique Coordinates: ', num_unique_coords))
  
  # filler NA result to return if needed
  NA_value <- tibble::tibble(address = rep(as.character(NA), num_unique_coords))
  names(NA_value)[1] <- address # rename column
  
  # If no valid coordinates are passed then return NA
  if (num_unique_coords == 1 && all(is.na(coord_pack$unique))) {
    if (verbose == TRUE) message(paste0('No valid coordinates found. Returning NA results.'))
    return(unpackage_inputs(coord_pack, NA_value, unique_only, return_coords))
  }
  
  # Exception for geocoder services that should default to single instead of batch
  if (method %in% pkg.globals$single_first_methods && mode != 'batch' ){
    mode <- 'single'
  }
  
  # Geocode coordinates one at a time in a loop -------------------------------------------------------
  if ((num_unique_coords > 1) & ((!(method %in% names(reverse_batch_func_map))) | (mode == 'single'))) {
    
    if (verbose == TRUE) {
      message('Executing single coordinate geocoding...\n')
    }
    
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
  if ((num_unique_coords > 1) || (mode == 'batch')) {
    if (verbose == TRUE) message('Executing batch geocoding...\n')
    
    if (is.null(limit) || (limit != 1 && return_coords == TRUE)) {
      stop('For batch geocoding (more than one coordinate per query) the limit argument must 
    be 1 (the default) OR the return_coords argument must be FALSE. Possible solutions:
    1) Set the mode argument to "single" to force single (not batch) geocoding 
    2) Set limit argument to 1 (ie. 1 result is returned per coordinate)
    3) Set return_coords to FALSE
    See the reverse_geo() function documentation for details.', call. = FALSE)
    }
    
    # set batch limit to default if not specified
    if (is.null(batch_limit)) batch_limit <- get_batch_limit(method)
    if (verbose == TRUE) message(paste0('Batch limit: ', 
                                        format(batch_limit, big.mark = ',')))
    
    # HERE: If a previous job is requested return_coords should be FALSE
    # This is because the job won't send the coords, but would recover the
    # results of a previous request
    if (method == 'here' && is.character(here_request_id) && return_coords == TRUE) {
      stop('HERE: When requesting a previous job via here_request_id, set return_coords to FALSE.
      See the geo() function documentation for details.', call. = FALSE)
    }
    
    # Enforce batch limit if needed
    if (num_unique_coords > batch_limit) {
      stop(paste0(format(num_unique_coords, big.mark = ','), ' unique coordinates found which exceeds the batch limit of',
            format(batch_limit, big.mark = ','), '.'), call. = FALSE)
    }
    
    if (verbose == TRUE) message(paste0('Passing ', 
                                        format(num_unique_coords, big.mark = ','), 
                                        ' coordinates to the ', method, ' batch geocoder'))
    
    # Convert our generic query parameters into parameters specific to our API (method)
    if (no_query == TRUE) return(unpackage_inputs(coord_pack, NA_value, 
                                                  unique_only, return_coords))
    
    # call the appropriate function for batch geocoding according the the reverse_batch_func_map named list
    # if batch limit was exceeded then apply that limit
    batch_results <- do.call(reverse_batch_func_map[[method]], 
        c(list(lat = coord_pack$unique$lat, long = coord_pack$unique$long),
        all_args[!names(all_args) %in% c('lat', 'long')]))
    
    # if verbose = TRUE, tell user how long batch query took
    if (verbose == TRUE) {
      batch_time_elapsed <- get_seconds_elapsed(start_time)
      print_time("Query completed in", batch_time_elapsed)
    }
    
    # map the raw results back to the original lat,long inputs that were passed if there are duplicates
    return(unpackage_inputs(coord_pack, batch_results, unique_only, return_coords))
  }
  
  ################################################################################
  #### Code past this point is for reverse geocoding a single coordinate     #####
  ################################################################################
  
  # Start to build 'generic' query as named list -----------------------------
  generic_query <- list()
  
  # Create parameters for lat, long coordinates
  custom_query <- get_coord_parameters(custom_query, method, lat, long)
  
  # Set API URL (if not already set) ----------------------------------------
  if (is.null(api_url)) {
    api_url <- get_api_url(method, reverse = TRUE, geocodio_v = geocodio_v, iq_region = iq_region,
                           mapbox_permanent = mapbox_permanent, mapquest_open = mapquest_open)
  }
  
  # Workaround for Mapbox/TomTom - The search_text should be in the url
  if (method %in%  c('mapbox', 'tomtom')) {
    api_url <- paste0(api_url, custom_query[["to_url"]], ".json")
    # Remove semicolons (Reserved for batch)
    api_url <- gsub(";", ",", api_url)
  }
  # Workaround for Bing - The search_text should be in the url
  if (method %in%  c('bing')) {
    api_url <- paste0(api_url, custom_query[["to_url"]])
  }
  # Set min_time if not set based on usage limit of service
  if (is.null(min_time)) min_time <- get_min_query_time(method)
  
  # add limit and api_key to generic query
  generic_query <- add_common_generic_parameters(generic_query, method, no_query, limit)

  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method, generic_query, custom_query)
  
  # Execute Single Coordinate Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  
  # if no_query = TRUE then return NA results
  if (no_query == TRUE) return(unpackage_inputs(coord_pack, 
                                                NA_value, 
                                                unique_only, return_coords))
  
  query_results <- query_api(api_url, api_query_parameters, method = method)
  
  if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(query_results$status)))
  
  ## Extract results -----------------------------------------------------------------------------------
  # if there were problems with the results then return NA
  if (query_results$status != 200) {
    extract_errors_from_results(method, query_results$content, verbose)
    results <- NA_value
  }
  else {
    results <- extract_reverse_results(method, jsonlite::fromJSON(query_results$content), full_results, flatten, limit)
    # rename address column
    names(results)[1] <- address
  }

  # Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  if (verbose == TRUE) message() # insert ending line break if verbose

  return(unpackage_inputs(coord_pack, results, unique_only, return_coords))
}