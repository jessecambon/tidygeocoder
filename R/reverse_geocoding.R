## References
# google: https://developers.google.com/maps/documentation/geocoding/start
# geocodio: https://www.geocod.io/docs/#reverse-geocoding
# osm: https://nominatim.org/release-docs/latest/api/Reverse/
# opencage: https://opencagedata.com/api

## NOTE: geocodio supports BATCH reverse geocoding

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

#' @export
#' lat, long = inputs
#' address = name of address column
reverse_geo <- function(lat, long, address = 'address', method = 'osm', limit = 1, api_url = NULL,
    full_results = FALSE, unique_only = FALSE, flatten = TRUE, verbose = FALSE, no_query = FALSE, 
    custom_query = list(), geocodio_v = 1.6, iq_region = 'us', param_error = TRUE) {

  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  
  # Reference Variables ------------------------------------------------------------

  # Check argument inputs
  stopifnot(is.numeric(lat), is.numeric(long), is.logical(verbose), is.logical(no_query), is.logical(flatten),
      is.logical(full_results), is.logical(unique_only), is.logical(param_error),
      is.numeric(limit), limit >= 1,  is.list(custom_query))
  
  if (no_query == TRUE) verbose <- TRUE
  start_time <- Sys.time() # start timer

  ################################################################################
  #### Code past this point is for reverse geocoding a single coordinate set #####
  ################################################################################
  
  # which methods require an api key (copied from geo() function)
  methods_requiring_api_key <- unique(tidygeocoder::api_parameter_reference[which(tidygeocoder::api_parameter_reference[['generic_name']] == 'api_key'), ][['method']])
  
  
  # Start to build 'generic' query as named list -------------------------
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
  
  # Set API URL (if not already set) ---------------------------
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
  
  # If API key is required then use the get_key() function to retrieve it
  if (method %in% methods_requiring_api_key) {
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

  combi_results <- dplyr::bind_cols(tibble(lat = lat, long = long), results)
  
  return(combi_results)
}


# a <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'osm', verbose = TRUE)
# b <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'google', full_results = TRUE, verbose = TRUE)
