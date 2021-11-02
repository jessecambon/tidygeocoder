
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

# call geo function with progress bar
progress_reverse_geo <- function(pb = NULL, ...) {
  results <- reverse_geo(...)
  if (!is.null(pb)) pb$tick()
  return(results)
}

# Create API parameters for a single set of coordinates (lat, long) based on the 
# method. Parameters are placed into the 'custom_query' variable which is a named list
# that is passed directly to the API service.
get_coord_parameters <- function(custom_query, method, lat, long) {
  if (method %in% c('osm', 'iq', 'geoapify')) {
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
#' longitudes must be between -180 and 180. Invalid values will not be sent to the geocoding service. 
#' The [reverse_geocode] function utilizes this function on coordinates contained in dataframes.
#' See example usage in `vignette("tidygeocoder")`.
#' 
#' Refer to [api_parameter_reference],
#' [min_time_reference], and [batch_limit_reference] for more details on 
#' geocoding service parameters and usage. 
#'
#' This function uses the [get_api_query], [query_api], and
#' [extract_reverse_results] functions to create, execute, and parse geocoder
#' API queries.
#' 
#' @param lat latitude values (input data)
#' @param long longitude values (input data)
#' @param method `r get_method_documentation(reverse = TRUE)`
#' 
#' @param address name of the address column (in the output data)
#' @param limit `r get_limit_documentation(reverse = TRUE, df_input = FALSE)`
#'
#' @param full_results `r get_full_results_documentation(reverse = TRUE)`
#' @param mode `r get_mode_documentation(reverse = TRUE)`
#' @param return_coords return input coordinates with results if TRUE. Note that
#'    most services return the input coordinates with `full_results = TRUE` and setting
#'    `return_coords` to FALSE does not prevent this.
#'
#' @param batch_limit `r get_batch_limit_documentation(reverse = TRUE)`
#' @inheritParams geo
#'     
#' @inherit geo return
#' @examples
#' \donttest{
#' options(tidygeocoder.progress_bar = FALSE)
#' 
#'  reverse_geo(lat = 38.895865, long = -77.0307713, method = 'osm')
#'  
#'  reverse_geo(
#'    lat = c(38.895865, 43.6534817, 300), 
#'    long = c(-77.0307713, -79.3839347, 600),
#'    method = 'osm', full_results = TRUE
#'  )
#'  
#' }
#' @seealso [reverse_geocode] [api_parameter_reference] [min_time_reference] [batch_limit_reference]
#' @export
reverse_geo <-
  function(
    lat,
    long,
    method = 'osm',
    address = 'address',
    limit = 1,
    full_results = FALSE,
    mode = '',
    unique_only = FALSE,
    return_coords = TRUE,
    min_time = NULL,
    progress_bar = show_progress_bar(),
    quiet = getOption("tidygeocoder.quiet", FALSE),
    api_url = NULL,
    timeout = 20,
    flatten = TRUE,
    batch_limit = NULL,
    verbose = getOption("tidygeocoder.verbose", FALSE),
    no_query = FALSE,
    custom_query = list(),
    api_options = list(),
    iq_region = 'us',
    geocodio_v = 1.6,
    mapbox_permanent = FALSE,
    here_request_id = NULL,
    mapquest_open = FALSE
  ) {
    
  # NSE eval
  address <- rm_quote(deparse(substitute(address)))
  
  # set the api_optons[["init"]] parameter if it is NULL
  api_options <- initialize_init(api_options)
  
  if (api_options[["init"]] == TRUE) {
    # Deprecate the iq_region argument
    if (!missing("iq_region")) {
      lifecycle::deprecate_warn("1.0.4", "reverse_geo(iq_region)", with = "reverse_geo(api_options)")
      api_options[["iq_region"]] <- iq_region
      iq_region <- NULL
    }
    
    # Deprecate the geocodio_v argument
    if (!missing("geocodio_v")) {
      lifecycle::deprecate_warn("1.0.4", "reverse_geo(geocodio_v)", with = "reverse_geo(api_options)")
      api_options[["geocodio_v"]] <- geocodio_v
      geocodio_v <- NULL
    }
    
    # Deprecate the mapbox_permanent argument
    if (!missing("mapbox_permanent")) {
      lifecycle::deprecate_warn("1.0.4", "reverse_geo(mapbox_permanent)", with = "reverse_geo(api_options)")
      api_options[["mapbox_permanent"]] <- mapbox_permanent
      mapbox_permanent <- NULL
    }
    
    # Deprecate the mapquest_open argument
    if (!missing("mapquest_open")) {
      lifecycle::deprecate_warn("1.0.4", "reverse_geo(mapquest_open)", with = "reverse_geo(api_options)")
      api_options[["mapquest_open"]] <- mapquest_open
      mapquest_open <- NULL
    }
    
    # Deprecate the here_request_id argument
    if (!missing("here_request_id")) {
      lifecycle::deprecate_warn("1.0.4", "reverse_geo(here_request_id)", with = "reverse_geo(api_options)")
      api_options[["here_request_id"]] <- here_request_id
      here_request_id <- NULL
    }
  }
  
  # apply api options defaults for options not specified by the user
  api_options <- apply_api_options_defaults(api_options)
  
  # capture all function arguments including default values as a named list.
  # IMPORTANT: make sure to put this statement before any other variables are defined in the function
  all_args <- as.list(environment())
  all_args$api_options[["init"]] <- FALSE # any following queries are not the initial query
  # remove NULL arguments
  all_args[sapply(all_args, is.null)] <- NULL
  
  # Check argument inputs
  stopifnot(is.logical(verbose), is.logical(no_query), is.logical(flatten),
      is.logical(full_results), is.logical(unique_only), is.logical(progress_bar), 
      is.logical(quiet),
      is.list(custom_query), 
      is.logical(api_options[["mapbox_permanent"]]),
      is.null(api_options[["here_request_id"]]) || is.character(api_options[["here_request_id"]]),
      is.logical(api_options[["mapquest_open"]]), is.logical(api_options[["geocodio_hipaa"]])
  )
  
  check_verbose_quiet(verbose, quiet, reverse = FALSE)
  
  # Check method argument
  
  check_api_options(api_options, 'reverse_geo')
  check_method(method, reverse = TRUE, mode, reverse_batch_func_map)
  
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
  
  # Exception for geocoding services that should default to single instead of batch
  if (method %in% pkg.globals$single_first_methods && mode != 'batch'){
    mode <- 'single'
  }
  
  # Geocode coordinates one at a time in a loop -------------------------------------------------------
  if ((api_options[["init"]] == TRUE) && (mode != 'batch') && ((!(method %in% names(reverse_batch_func_map))) || (num_unique_coords == 1) || (mode == 'single'))) {
    
    # construct arguments for a single address query
    # note that non-lat/long related fields go to the MoreArgs argument of mapply
    # since we aren't iterating through them
    single_coord_args <- c(
      list(FUN = progress_reverse_geo, lat = coord_pack$unique$lat, long = coord_pack$unique$long),
      list(MoreArgs = all_args[!names(all_args) %in% c('lat', 'long')],
           USE.NAMES = FALSE, SIMPLIFY = FALSE)
    )
    
    if (quiet == FALSE) {
      query_start_message(method, num_unique_coords, reverse = TRUE, batch = FALSE)
    }
    
    if (progress_bar == TRUE) {
      
      # intialize progress bar 
      pb <- create_progress_bar(
        num_unique_coords
      )
      # add progress bar object to query
      single_coord_args$MoreArgs$pb <- pb
    } 
    
    # Reverse geocode each coordinate individually by recalling this function with mapply
    list_coords <- do.call(mapply, single_coord_args)
    
    # tell user how long the query took if the progress bar hasn't already
    if (quiet == FALSE && progress_bar == FALSE) {
      query_complete_message(start_time)
    }
    
    # rbind the list of tibble dataframes together
    stacked_results <- dplyr::bind_rows(list_coords)
    
    # note that return_inputs has been set to FALSE here since lat/long coordinates will already
    # be returned in the first geo function call (if asked for)
    return(unpackage_inputs(coord_pack, stacked_results, unique_only, FALSE))
  }
  
  # Batch geocoding --------------------------------------------------------------------------
  if (api_options[["init"]] == TRUE) {
    if (verbose == TRUE) message('Executing batch geocoding...\n')
    
    # check for conflict between limit and return_coords arguments
    check_limit_for_batch(limit, return_coords, reverse = TRUE)
    
    # set batch limit to default if not specified
    if (is.null(batch_limit)) batch_limit <- get_batch_limit(method)
    if (verbose == TRUE) message(paste0('Batch limit: ', 
                                        format(batch_limit, big.mark = ',')))
    
    if (method == 'here') check_here_return_input(api_options[["here_request_id"]], return_coords, reverse = TRUE)
    
    # Enforce batch limit if needed
    if (num_unique_coords > batch_limit) {
      stop(paste0(format(num_unique_coords, big.mark = ','), 
          ' unique coordinates found which exceeds the batch limit of ',
            format(batch_limit, big.mark = ','), '.'), call. = FALSE)
    }
    
    # Display message to user on the batch query
    if (quiet == FALSE) {
      query_start_message(
        method, 
        num_unique_coords,
        reverse = TRUE,
        batch = TRUE,
        display_time = FALSE
      )
    }
    
    # Convert our generic query parameters into parameters specific to our API (method)
    if (no_query == TRUE) return(unpackage_inputs(coord_pack, NA_value, 
                                                  unique_only, return_coords))
    
    # call the appropriate function for batch geocoding according the the reverse_batch_func_map named list
    # if batch limit was exceeded then apply that limit
    batch_results <- do.call(reverse_batch_func_map[[method]], 
        c(list(lat = coord_pack$unique$lat, long = coord_pack$unique$long),
        all_args[!names(all_args) %in% c('lat', 'long')]))
    
    # if verbose = FALSE, tell user how long batch query took
    if (quiet == FALSE) {
      query_complete_message(start_time)
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
  custom_query <- get_coord_parameters(
    custom_query, 
    method,
    coord_pack$unique$lat,
    coord_pack$unique$long
    )
  
  # Set API URL (if not already set) ----------------------------------------
  if (is.null(api_url)) {
    api_url <- get_api_url(method, reverse = TRUE, geocodio_v = api_options[["geocodio_v"]], iq_region = api_options[["iq_region"]],
                           mapbox_permanent = api_options[["mapbox_permanent"]], 
                           mapquest_open = api_options[["mapquest_open"]], geocodio_hipaa = api_options[["geocodio_hipaa"]])
  }
  
  ## Workaround for Mapbox/TomTom - The search_text should be in the url
  api_url <- api_url_modification(method, api_url, generic_query, custom_query, reverse = TRUE)
  
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