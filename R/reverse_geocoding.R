# list of methods supporting reverse geocoding
reverse_methods <- c('osm', 'geocodio', 'opencage', 'iq')

#' @export
reverse_geo <- function(lat, long, method = 'osm', limit = 1, api_url = NULL,
    full_results = FALSE, unique_only = FALSE, flatten = TRUE, verbose = FALSE, no_query = FALSE, 
    custom_query = list(), geocodio_v = 1.6, param_error = TRUE) {

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
  
  # Make Lat long args ------------------------------------------
  
  # METHOD 1: lat = 123, lon = 123
    # osm, iq
  # METHOD 2:  q = lat,lon
    # geocodio, opencage
  
  if (method %in% c('osm', 'iq')) {
    custom_query[['lat']] <- lat
    custom_query[['lon']] <- long
  } else if (method %in% c('geocodio', 'opencage')) {
    custom_query[['q']] <-  paste0(as.character(lat), ',', as.character(long))
  }
  
  # Set API URL (if not already set) ---------------------------
  if (is.null(api_url)) {
    api_url <- switch(method,
                      "osm" = 'https://nominatim.openstreetmap.org/reverse',
                      "geocodio" = 'https://api.geocod.io/v1.6/reverse',
                      "iq" = 'https://us1.locationiq.com/v1/reverse.php',
                      "opencage" = get_opencage_url() # same url as forward geocoding
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
  
  display_query(api_url, api_query_parameters)

  ## Extract results -----------------------------------------------------------------------------------

}
