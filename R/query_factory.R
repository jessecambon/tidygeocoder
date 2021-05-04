### Functions for constructing API queries

# Get API Key from environmental variables
get_key <- function(method) {
  
  # which environmental variable are we loading the api key from?
  env_var <- get_setting_value(tidygeocoder::api_key_reference, method, 'env_var')

  # load api key from environmental variable
  key <- Sys.getenv(env_var)
  
  if (key == "") stop(paste0("An API Key is needed to use the '", method, "' method.
    Set the \"", env_var, "\" variable in your .Renviron file or with Sys.setenv().
    Tip: usethis::edit_r_environ() will open your .Renviron file for editing. "), call. = FALSE)
  else return(key)
}

# return minimum number of seconds each query should take based on usage rate limits
get_min_query_time <- function(method) {
  
  # Get min_time from min_time_reference. If method not found then default to 0
  if (method %in% tidygeocoder::min_time_reference[['method']]) {
    min_time <- get_setting_value(tidygeocoder::min_time_reference, method, 'min_time')
  } else {
    min_time <- 0
  }
  return(min_time)
}

# Return the batch geocoding limit for the service
# this is called from geo() and reverse_geo() when batch_limit = NULL
get_batch_limit <- function(method) {
  return(
    get_setting_value(tidygeocoder::batch_limit_reference, method, 'batch_limit')
  )
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
#' are converted into api parameters using the  [api_parameter_reference] 
#' dataset. 
#' 
#' The [query_api] function executes the queries created 
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
#' @seealso [query_api] [api_parameter_reference] [geo] [reverse_geo]
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
  # Bing: Workaround to remove inputs from parameters (since it is added to the API url instead)
  if (method == "bing") {
    api_query_parameters <-
      api_query_parameters[!names(api_query_parameters) %in% c("to_url")]
  }
  return(api_query_parameters)
}

#' Execute a geocoder API query
#' 
#' @description
#' The [get_api_query] function can create queries for this
#' function to execute.  
#' 
#' @param api_url Base URL of the API. query parameters are appended to this
#' @param query_parameters api query parameters in the form of a named list
#' @param mode determines the type of query to execute
#' 
#'     - "single": geocode a single input (all methods)
#'     - "list": batch geocode a list of inputs (ex. geocodio)
#'     - "file": batch geocode a file of inputs (ex. census)
#'     
#' @param batch_file a csv file of input data to upload (for `mode = 'file'`)
#' @param input_list a list of input data (for `mode = 'list'`)
#' @param content_encoding Encoding to be used for parsing content
#' @param timeout timeout in minutes
#' @param method if 'mapquest' or 'arcgis' then the query status code is changed appropriately
#' @return a named list containing the response content (`content`) and the HTTP request status (`status`)
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
#' @seealso [get_api_query] [extract_results] [extract_reverse_results] [geo] [reverse_geo]
#' @export 
query_api <- function(api_url, query_parameters, mode = 'single', 
          batch_file = NULL, input_list = NULL, content_encoding = 'UTF-8', timeout = 20, method = '') {
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
  if (mode == 'single' && method == 'mapquest' && httr::status_code(response) == 200) {
    status_code <- jsonlite::fromJSON(content)$info$statuscode
    if (status_code == 0) status_code <- 200
    
    httr::warn_for_status(status_code)
    return(list(content = content, status = status_code))
  }
  
  # ArcGIS exception 
  # Need to extract from results
  if (method == 'arcgis' && httr::status_code(response) == 200) {
    raw_results <- jsonlite::fromJSON(content)
    status_code <- 200
    if ('error' %in% names(raw_results)){
      status_code <- raw_results$error$code
      # One error, 498, not standard. Switching to 401      
      if (status_code == 498) status_code <- 401
    }
    
    httr::warn_for_status(status_code)
    return(list(content = content, status = status_code))
  }

  return(list(
      content = content, 
      status = httr::status_code(response)
    ))
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
  all_params <- get_setting_value(tidygeocoder::api_parameter_reference, method, 'generic_name')
  
  if (address_only == TRUE) {
    return(all_params[all_params %in% pkg.globals$address_arg_names])
  } else {
    return(all_params)
  }
}

# get list of services that require an api key according to api_parameter_reference
get_services_requiring_key <- function() {
  return(
    unique(tidygeocoder::api_parameter_reference[which(tidygeocoder::api_parameter_reference[['generic_name']] == 'api_key'), ][['method']])
    )
}

# this function adds common generic
add_common_generic_parameters <- function(generic_query, method, no_query, limit) {
  # If API key is required then use the get_key() function to retrieve it
  if (method %in% get_services_requiring_key()) {
    
    # don't attempt to load API key if no_query = TRUE. This allows you to
    # run no_query = TRUE without having the API key loaded.
    if (no_query == TRUE) generic_query[['api_key']] <- 'xxxxxxxxxx'
    else generic_query[['api_key']] <- get_key(method)
  }
  # add limit parameter
  if (!is.null(limit)) generic_query[['limit']] <- limit
  
  return(generic_query)
}