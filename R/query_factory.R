### Functions for constructing API queries

# Get API Key from environmental variables
get_key <- function(method) {
  # define environmental variable name
  env_var <- switch(method,
         'geocodio' = "GEOCODIO_API_KEY",
         'iq' = "LOCATIONIQ_API_KEY",
         'google' = "GOOGLEGEOCODE_API_KEY",
         'opencage' = "OPENCAGE_KEY",
         'mapbox' = "MAPBOX_API_KEY",
         'here' = "HERE_API_KEY",
         'tomtom' = "TOMTOM_API_KEY",
         'mapquest' = "MAPQUEST_API_KEY",
         )
  # load api key from environmental variable
  key <- Sys.getenv(env_var)
  
  if (key == "") stop(paste0("An API Key is needed to use the '", method, "' method.
    Set the \"", env_var, "\" variable in your .Renviron file or with Sys.getenv()."))
  else return(key)
}

# return minimum number of seconds each query should take based on usage rate limits
get_min_query_time <- function(method) {
  
  # stores the minimum number of seconds that must elapse for each query
  # based on the usage limit of the service (free tier if there are multiple plans available)
  # Stored value is SECONDS PER QUERY
  seconds_per_query <- list(
    osm = 1,               # 1 query/second             
    geocodio = 60/1000,    # 1000 queries per minute (free tier)
    iq = 1/2,              # 2 queries per second (free tier)
    google = 1/50,         # 50 queries per second
    opencage = 1,          # 1 query/second 
    mapbox = 60/600,       # 600 queries per minute (free tier)
    tomtom = 1/5           # 5 queries per second (free tier)
  )
  
  # default min_time to 0
  min_time <- ifelse(method %in% names(seconds_per_query), seconds_per_query[[method]], 0)
  return(min_time)
}

# API URL Functions ----------------------------------------------------------------
# reverse = TRUE for reverse geocoding


# return : returntype => 'locations' or 'geographies'
# search:  searchtype => 'onelineaddress', 'addressbatch', 'address', or 'coordinates'
get_census_url <- function(return_type, search) {
  return(paste0("https://geocoding.geo.census.gov/geocoder/", return_type, "/", search))
}

get_geocodio_url <- function(api_v, reverse = FALSE) {
  # return API URL based on api version (ex. 1.6)
  url_keyword <- if (reverse == TRUE) 'reverse' else 'geocode'
  
  return(paste0("https://api.geocod.io/v", as.character(api_v), "/", url_keyword))
}

get_osm_url <- function(reverse = FALSE) {
  url_keyword <- if (reverse == TRUE) 'reverse' else 'search'
  
  return(paste0('https://nominatim.openstreetmap.org/', url_keyword))
}

get_iq_url <- function(region = 'us', reverse = FALSE) {
  # region can be 'us' or 'eu'
  
  url_keyword <- if (reverse == TRUE) 'reverse' else 'search'
  
  return(paste0("https://", region, "1.locationiq.com/v1/", url_keyword,  ".php"))
}

get_google_url <- function() return("https://maps.googleapis.com/maps/api/geocode/json")

get_opencage_url <- function() return("https://api.opencagedata.com/geocode/v1/json")

get_mapbox_url <- function(mapbox_permanent = FALSE) {
  endpoint <- if (mapbox_permanent == TRUE) "mapbox.places-permanent" else "mapbox.places"
  return(paste0("https://api.mapbox.com/geocoding/v5/", endpoint, "/"))
}

get_here_url <- function(reverse = FALSE) {
  if (reverse == TRUE) return("https://revgeocode.search.hereapi.com/v1/revgeocode")
  return("https://geocode.search.hereapi.com/v1/geocode")
}

get_tomtom_url <- function(reverse = FALSE) {
    url_keyword <- if (reverse == TRUE) 'reverseGeocode/' else 'geocode/'
    return(paste0('https://api.tomtom.com/search/2/', url_keyword))
}

get_mapquest_url <- function(mapquest_open = FALSE, reverse = FALSE) {
  endpoint <- if (mapquest_open == TRUE) 'http://open.' else 'http://www.'
  url_keyword <- if (reverse == TRUE) 'reverse' else 'address'
  return(paste0(endpoint,'mapquestapi.com/geocoding/v1/', url_keyword))
}

## wrapper function for above functions
### IMPORTANT: if arguments are changed in this definition then make sure to 
### update reverse_geo.R and geo.R where this function is called.
get_api_url <- function(method, reverse = FALSE, return_type = 'locations',
            search = 'onelineaddress', geocodio_v = 1.6, iq_region = 'us', 
            mapbox_permanent = FALSE, mapquest_open = FALSE) {
  api_url <- switch(method,
         "osm" = get_osm_url(reverse = reverse),
         "census" = get_census_url(return_type, search),
         "geocodio" = get_geocodio_url(geocodio_v, reverse = reverse),
         "iq" = get_iq_url(iq_region, reverse = reverse),
         "opencage" = get_opencage_url(), # same url as forward geocoding
         "google" = get_google_url(), # same url as forward geocoding
         "mapbox" = get_mapbox_url(mapbox_permanent), # same url as fwd geocoding
         "here" = get_here_url(reverse = reverse),
         "tomtom" = get_tomtom_url(reverse = reverse),
         "mapquest" = get_mapquest_url(mapquest_open, reverse = reverse),
         )


  if (length(api_url) == 0) stop('API URL not found')
  return(api_url)
}

# API Parameters ----------------------------------------------------------------

# Create an API-specific parameter for a given method
# given generic parameter name and a value
create_api_parameter <- function(method_name, param_name, value) {
  api_ref <- tidygeocoder::api_parameter_reference
  
  # Extract the API specific parameter name
  api_parameter_name <- 
    api_ref[which( (api_ref$method == method_name) &
        (api_ref$generic_name ==  param_name)), 'api_name'][[1]]
  
  # If api_parameter_name is NA or missing then return empty list
  if ((length(api_parameter_name) == 0)) return(list())
  if (is.na(api_parameter_name)) return(list())
  
  param <- list()
  param[[api_parameter_name]] <- value
  return(param)
}

#' Construct a geocoder API query
#' 
#' @description 
#' The geocoder API query is created using universal "generic" parameters
#' and optional api-specific "custom" parameters. Generic parameters
#' are converted into api parameters using the  \code{\link{api_parameter_reference}} 
#' dataset. 
#' 
#' The \code{\link{query_api}} function executes the queries created 
#' by this function.
#'  
#' @param method method name (ie. 'census')
#' @param generic_parameters universal 'generic' parameters
#' @param custom_parameters custom api-specific parameters
#' @return API parameters as a named list
#' @examples
#' get_api_query("osm", list(address = 'Hanoi, Vietnam'))
#' 
#' get_api_query("census", list(street = '11 Wall St', city = "NY", state = 'NY'),
#'   list(benchmark = "Public_AR_Census2010"))
#'
#' @seealso \code{\link{query_api}} \code{\link{geo}} \code{\link{api_parameter_reference}} 
#' @export
get_api_query <- function(method, generic_parameters = list(), custom_parameters = list() ) {
  api_ref <- tidygeocoder::api_parameter_reference
  
  # create the "main" api parameters from the passed generic parameters
  main_api_parameters <- list()
  for (generic_parameter_name in names(generic_parameters)) {
    main_api_parameters <- c(main_api_parameters, 
      create_api_parameter(method, generic_parameter_name,
      generic_parameters[[generic_parameter_name]])
      )
  }
  
  # Allow custom API parameters to overwrite the generic parameters but throw a warning
  for (custom_name in names(custom_parameters)) {
    if (custom_name %in% names(main_api_parameters)) {
      warning(paste0("Custom API Parameter '", custom_name, "' was already specified"))
      main_api_parameters[[custom_name]] <- NULL # remove the parameter from main parameters
    }
  }
  
  # Extract default parameter values ----------------------------------------------------------
  # only extract values that have default values and don't already exist in main_api_parameters
  default_api_parameters <- tibble::deframe(
    api_ref[which(api_ref[['method']] == method &
      api_ref$required == TRUE &
      !is.na(api_ref$default_value) &
      !api_ref$api_name %in% names(c(main_api_parameters, custom_parameters))), ][c('api_name','default_value')]
    )
  
  # Combine address, api_key, and default parameters for full query
  api_query_parameters <- c(main_api_parameters, custom_parameters, default_api_parameters)
  
  # Mapbox: Workaround to remove inputs from parameters (since it is added to the API url instead)
  if (method == "mapbox") {
    api_query_parameters <-
      api_query_parameters[!names(api_query_parameters) %in% c("search_text", "to_url")]
  }
  # TomTom: Workaround to remove inputs from parameters (since it is added to the API url instead)
    if (method == "tomtom") {
    api_query_parameters <-
      api_query_parameters[!names(api_query_parameters) %in% c("query", "to_url")]
  }
  return(api_query_parameters)
}

#' Execute a geocoder API query
#' 
#' @description
#' The \code{\link{get_api_query}} function can create queries for this
#' function to execute.  
#' 
#' @param api_url Base URL of the API. query parameters are appended to this
#' @param query_parameters api query parameters in the form of a named list
#' @param mode 
#' \itemize{
#'     \item \code{"single"} : geocode a single address (all methods)
#'     \item \code{"list"} : batch geocode a list of addresses (geocodio)
#'     \item \code{"file"} : batch geocode a file of addresses (census)
#' }
#' @param batch_file a csv file of addresses to upload (census)
#' @param input_list a list of addresses or latitude, longitude coordinates for batch geocoding (geocodio)
#' should be 'json' for geocodio and 'multipart' for census 
#' @param content_encoding Encoding to be used for parsing content
#' @param timeout timeout in minutes
#' @return a named list containing the response content (\code{content}) and the HTTP request status (\code{status})
#' @examples
#' \donttest{
#' raw1 <- query_api("http://nominatim.openstreetmap.org/search", 
#'    get_api_query("osm", list(address = 'Hanoi, Vietnam')))
#'    
#' raw1$status
#'    
#' extract_results('osm', jsonlite::fromJSON(raw1$content))
#' 
#' raw2 <- query_api("http://nominatim.openstreetmap.org/reverse", 
#'    get_api_query("osm", custom_parameters = list(lat = 38.895865, lon = -77.0307713)))
#'    
#' extract_reverse_results('osm', jsonlite::fromJSON(raw2$content))
#' }
#' 
#' @seealso \code{\link{get_api_query}} \code{\link{extract_results}} \code{\link{geo}}
#' @export 
query_api <- function(api_url, query_parameters, mode = 'single', 
          batch_file = NULL, input_list = NULL, content_encoding = 'UTF-8', timeout = 20) {
   response <- switch(mode,
    'single' = httr::GET(api_url, query = query_parameters),
    'list' = httr::POST(api_url, query = query_parameters, 
          body = as.list(input_list), encode = 'json', httr::timeout(60 * timeout)),
    'file' = httr::POST(api_url,  
          body = c(list(addressFile = httr::upload_file(batch_file)), query_parameters),
          encode = 'multipart',
          httr::timeout(60 * timeout))
   )
  
  httr::warn_for_status(response)
  
  content <- httr::content(response, as = 'text', encoding = content_encoding)
  
  # MapQuest exception on GET
  # When a valid key is passed, errors on query are in the response
  # Succesful code in response is 0
  # https://developer.mapquest.com/documentation/geocoding-api/status-codes/
  if (mode == 'single' && 
      isTRUE(grep('mapquest', api_url) > 0) && 
      isTRUE(httr::status_code(response) == 200)) {
    status_code <- jsonlite::fromJSON(content)$info$statuscode
    if (status_code == 0) status_code <- 200
    
    httr::warn_for_status(status_code)
    return(list(content = content, status = status_code))
  }
  
  return(list(content = content, status = httr::status_code(response)))
}

# Functions for displaying API queries (when verbose = TRUE) ----------------------

# print values in a named list (used for displaying query parameters)
display_named_list <- function(named_list) {
  # unique parameter names for all api keys
  api_key_names <- unique(tidygeocoder::api_parameter_reference[which(tidygeocoder::api_parameter_reference[['generic_name']] == 'api_key'), ][['api_name']])
  
  for (var in names(named_list)) {
    ## censor API Key values
    if (var %in% api_key_names) {
      named_list[var] <- paste(rep('x', nchar(named_list[var])), collapse ='')
    }
    message(paste0(var, ' : "', named_list[var], '"'))
  }
  message('')
}

# Displays query URL and parameters
display_query <- function(api_url, api_query_parameters) {
  message(paste0('Querying API URL: ', api_url))
  message('Passing the following parameters to the API:')
  display_named_list(api_query_parameters)
}

# Get the legal generic parameters (ie. address, city, limit, etc.)
# for a method (don't call with method = 'cascade')
# if address_only = TRUE then limit to address parameters (street, city, address, etc.)
get_generic_parameters <- function(method, address_only = FALSE) {
  all_params <- tidygeocoder::api_parameter_reference[which(tidygeocoder::api_parameter_reference[['method']] == method), ][['generic_name']]
  
  if (address_only == TRUE) {
    return(all_params[all_params %in% pkg.globals$address_arg_names])
  } else {
    return(all_params)
  }
}

# get list of services that require an api key
get_services_requiring_key <- function() {
  return(unique(tidygeocoder::api_parameter_reference[which(tidygeocoder::api_parameter_reference[['generic_name']] == 'api_key'), ][['method']]))
}

# this function adds common generic
add_common_generic_parameters <- function(generic_query, method, no_query, limit) {
  # If API key is required then use the get_key() function to retrieve it
  if (method %in% get_services_requiring_key()) {
    
    # don't attempt to load API key if no_query = TRUE. This allows you to
    # run no_query = TRUE without having the API key loaded.
    if (no_query == TRUE) generic_query[['api_key']] <- 'xxxxxxxxxxxxx'
    else generic_query[['api_key']] <- get_key(method)
  }
  
  # add limit parameter
  if (!is.null(limit)) generic_query[['limit']] <- limit
  
  return(generic_query)
}