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

extract_reverse_results <- function(method, response, full_results = TRUE, flatten = TRUE) {
  # extract the single line address
  address <- switch(method,
                    'osm' = response['display_name'],
                    'iq' = response['display_name'],
                    'geocodio' = response$results['formatted_address'],
                    'google' = response$results['formatted_address'][1, ],
                    'opencage' = response$results['formatted']
  )
  
  # extract other results (besides single line address)
  if (full_results == TRUE) {
    results <- switch(method,
                      # !!!!WARNING!!!! - osm, iq results currently excludes boundingbox and address components
                      'osm' = response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))],
                      'iq' =  response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))],
                      'geocodio' = response$results[!names(response$results) %in% c('formatted_address')],
                      'google' = response$results[1, ], # take first row of multiple results for now
                      'opencage' = response$results[!names(response$results) %in% c('formatted')]
    )
    
    combined_results <- dplyr::bind_cols(address, results)
  } else {
    combined_results <- address
  }
  
  combined_results <- tibble::as_tibble(combined_results)
  
  if (flatten == TRUE) return(jsonlite::flatten(combined_results))
  else return(combined_results)
}

#' lat, long = inputs
#' address = name of address column
#' @export
reverse_geo <- function(lat, long, address = address, method = 'osm', limit = 1, api_url = NULL, return_coords = TRUE,
    full_results = FALSE, unique_only = FALSE, flatten = TRUE, verbose = FALSE, no_query = FALSE, mode = '',
    custom_query = list(), geocodio_v = 1.6, iq_region = 'us', param_error = TRUE, batch_limit = 10000) {

  # NSE eval
  address <- rm_quote(deparse(substitute(address)))
  
  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  
  # Reference Variables ------------------------------------------------------------

  # Check argument inputs
  stopifnot(is.numeric(lat), is.numeric(long), is.logical(verbose), is.logical(no_query), is.logical(flatten),
      is.logical(full_results), is.logical(unique_only), is.logical(param_error),
      is.numeric(limit), limit >= 1,  is.list(custom_query))
  
  if (length(lat) != length(long)) stop('Lengths of lat and long must be equal.')
  num_coords <- length(lat)
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer
  
  
  # If multiple coordinates are given, recursively call this function in a loop
  if ((num_coords > 1) & ((!(method %in% names(reverse_batch_func_map))) | (mode == 'single'))) {
    # construct arguments for a single address query
    # note that non-address related fields go to the MoreArgs argument of mapply
    # since we aren't iterating through them
    single_coord_args <- c(
      list(FUN = reverse_geo, lat = lat, long = long),
      list(MoreArgs = all_args[!names(all_args) %in% c('lat', 'long')],
           USE.NAMES = FALSE, SIMPLIFY = FALSE)
    )
    
    #print(single_coord_args)
    
    # Reverse geocode each coordinate individually by recalling this function with mapply
    list_coords <- do.call(mapply, single_coord_args)
    # rbind the list of tibble dataframes together
    stacked_results <- dplyr::bind_rows(list_coords)
    
    return(stacked_results)
  }
  
  # Batch geocoding --------------------------------------------------------------------------
  if ((num_coords > 1) | (mode == 'batch')) {
    
    if (verbose == TRUE) message(paste0('Passing ', 
            format(min(batch_limit, num_coords), big.mark = ','), 
            ' addresses to the ', method, ' batch geocoder'))
    
    # call the appropriate function for batch geocoding according the the batch_func_map named list
    # if batch limit was exceeded then apply that limit
    batch_results <- do.call(reverse_batch_func_map[[method]], c(list(lat = lat, long = long),
        all_args[!names(all_args) %in% c('lat', 'long')]))
    
    if (return_coords == TRUE) return(dplyr::bind_cols(tibble::tibble(lat = lat, long = long), batch_results))
    else return(batch_results)
  }
  

  ################################################################################
  #### Code past this point is for reverse geocoding a single coordinate set #####
  ################################################################################
  
  # Start to build 'generic' query as named list -----------------------------
  generic_query <- list()
  
  # Create Lat/Long argument(s)
  # METHOD 1: lat = 123, lon = 123
    # osm, iq
  # METHOD 2:  q = lat,lon
    # geocodio, opencage
  # METHOD 3: latlng = lat,lon
    # google
  
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
  
  # Set API URL (if not already set) ----------------------------------------
  if (is.null(api_url)) {
    api_url <- switch(method,
                      "osm" = get_osm_url(reverse = TRUE),
                      "geocodio" = get_geocodio_url(geocodio_v, reverse = TRUE),
                      "iq" = get_iq_url(iq_region, reverse = TRUE),
                      "opencage" = get_opencage_url(), # same url as forward geocoding
                      "google" = get_google_url() # same url as forward geocoding
    )
  }
  if (length(api_url) == 0) stop('API URL not found')
  
  if (!is.null(limit)) generic_query[['limit']] <- limit
  
  # If API key is required then use the get_key() function to retrieve it
  if (method %in% get_services_requiring_key()) {
    generic_query[['api_key']] <- get_key(method)
  }
  
  if (!is.null(limit)) generic_query[['limit']] <- limit
  
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method, generic_query, custom_query)
  
  # Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) display_query(api_url, api_query_parameters)
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  
  ## Extract results -----------------------------------------------------------------------------------
  results <- extract_reverse_results(method, raw_results, full_results, flatten)
  # rename address column
  names(results)[1] <- address

  if (return_coords == TRUE) return(dplyr::bind_cols(tibble::tibble(lat = lat, long = long), results))
  else return(results)
  
}
  


# a <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'osm', verbose = TRUE)
# b <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'google', full_results = TRUE, verbose = TRUE)

# c <- reverse_geo(lat = c(38.895865, 43.6534817, 300), long = c(-77.0307713, -79.3839347, 600), method = 'geocodio', full_results = TRUE, verbose = TRUE)