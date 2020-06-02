# Query can be customized or left to default according to method (service) chosen. 
# full_results : if true return all info provided by geocoder (not just lat/long)
## Set defaults for query parameters but allow user to override them. 
## Include address and limit as a query parameter.
# Either address of query_parameters must be defined

#' Workhorse function for geocoding
#' @param method the geocoder function you want to use
#' \itemize{
#'   \item "census": US Census Geocoder. US street-level addresses only.
#'   \item "osm": Nominatim (OSM). Worldwide coverage.
#' }
#' @param street street address
#' @return parsed results from geocoder
#' @export
geo <- function(street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL,
    method = 'census', lat = lat, long = long,
    limit=1, api_url=NULL, custom_query=list(),
    full_results=FALSE, verbose=FALSE, min_time=NULL) {
  
  # NSE - Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  print('lat: ')
  print(lat)
  print('long: ')
  print(long)
  print("verbose: ")
  print(verbose)
  
  # capture all function arguments including default values as a named list.
  # make sure to put this before any other variables are defined
  all_args <- as.list(environment())
  
  # names of all address component fields
  address_arg_names <- c('street', 'city', 'county', 'state', 'postalcode', 'country')
  start_time <- Sys.time() # start timer
  
  ## Extract the address components
  address_components <- all_args[names(all_args) %in% address_arg_names]
  address_components[sapply(address_components, is.null)] <- NULL # remove NULL items
  
  address_component_lengths <- sapply(address_components, length)
  if (max(address_component_lengths) != min(address_component_lengths)) {
    stop('Address components must be equal in length')
  } 
  num_addresses <- max(address_component_lengths)
  
  # If method='cascade' is called then pass all function arguments 
  # except for method to geo_cascade and return the results 
  if (method == 'cascade') return(do.call(geo_cascade,all_args[names(all_args) != 'method']))
  
  ### Set min_time if not set
  if (method %in% c('osm','iq') & is.null(min_time))  min_time <- 1 
  else if (is.null(min_time)) min_time <- 0
  
  ### Build Generic query as named list ---------------------------
  generic_query <- list()
  if (method %in% c('geocodio','iq')) {
    generic_query[['api_key']] <- get_key(method)
    if (generic_query[['api_key']] == '') stop("API Key must be defined")
  }
  if (!is.null(limit))                generic_query[['limit']]   <- limit
  
  ### If more than one adress is passed then either call a batch geocoder function or recall 
  ### this function repeatedly for each individual address
  if (num_addresses > 1) {
    if (method %in% c('osm', 'iq')) {
      # construct args for single address query
      # note that non-address related fields go to the MoreArgs argument of mapply
      # since we aren't interating through them
      single_addr_args <- c(
        list(FUN = geo), 
        all_args[names(all_args) %in% address_arg_names],
        MoreArgs = all_args[!names(all_args) %in% address_arg_names],
        list(USE.NAMES = FALSE, SIMPLIFY = FALSE)
      )
      single_addr_args <- single_addr_args[lengths(single_addr_args) != 0]  # remove NULL and 0 length items
      
      print('single_addr_args:')
      print(single_addr_args)
      
      # Geocode each address individually by recalling this function with lapply
      list_coords <- do.call(mapply, single_addr_args)
      
      print('class(list_coords) : ')
      print(class(list_coords))
      print('list_coords: ')
      print(list_coords)
        
      # rbind the list of tibble dataframes together
      coordinates <- dplyr::bind_rows(list_coords)
      return(coordinates)
      
      #message(paste0('Batch geocoding not available for ', method, '. Pass a single address.'))
    } else {
      ### Call Batch Geocoding
      if (verbose == TRUE) message(paste0('Calling the ', method, 'batch geocoder'))
      # Convert our generic query parameters into parameters specific to our API (method)
      api_query_parameters <- get_api_query(method,generic_query)
      return(switch(method,
                    'census' = batch_census(street, full_results = full_results),
                    'geocodio' = batch_geocodio(street, full_results = full_results)
                    ))
    }
  }
  
  #### Code past this point is for geocoding a single address
  # what to return when we don't find results
  NA_value <- get_na_value(lat,long)
  print('NA_value:')
  print(NA_value)
  
  if (!is.null(street)) generic_query[['address']] <- street
  # Convert our generic query parameters into parameters specific to our API (method)
  api_query_parameters <- get_api_query(method,generic_query)
  
  # If there is no custom query then we are relying on the address and it must
  # be non-missing/NA
  
  # Check if address NULL
  if ((length(custom_query) == 0) & is.null(street)) {
    if (verbose == TRUE) message("Blank or missing address!")
    return(NA_value)    
  }
  
  # Check if address is missing/NA
  if ((length(custom_query) == 0) & (is.na(street) | trimws(street) == "")) {
    if (verbose == TRUE) message("Blank or missing address!")
    return(NA_value)
  }
  
  
  # If api_url is not set then define it automatically.
  # If it's defined then figure out if its a URL (http...) 
  if (is.null(api_url)) api_url <- get_api_url(method) 
  else api_url <- get_api_url(method,url_name = trimws(api_url))
  
  ## TOdo --- implement custom URL field where user can specify the entire URL address
  #  else if (stringr::str_detect(stringr::str_trim(api_url),'^http'))  api_url <- api_url
  
  if (length(api_url) == 0) {
    warning('API URL not found')
    return(NA_value)
  }
  
  ### Execute Single Address Query -----------------------------------------
  if (verbose == TRUE) message(paste0('Querying API URL: ', api_url))
  raw_results <- jsonlite::fromJSON(query_api(api_url, api_query_parameters))
  
  # If no results found, return NA
  if (length(raw_results) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NA_value)
  }
  
  ### Extract lat/long as an unnamed numeric vector c(lat,long)
  coords <- extract_coords(method,raw_results)
  
  if (length(coords) == 0) {
    if (verbose == TRUE) message("No results found")
    return(NA_value)
  }
  
  # Convert numeric vector to tibble
  names(coords) <- c(lat, long)
  
  print('names of coords:')
  print(names(coords))
  
  coords_tibble <- tibble::as_tibble_row(coords)
  
  ### Make sure the proper amount of time has elapsed for the query per min_time
  pause_until(start_time, min_time, debug = verbose) 
  
  ### Return  results
  return(coords_tibble)
}