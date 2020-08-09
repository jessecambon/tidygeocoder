### Functions for constructing API queries

# Get API Key from environmental variables
get_key <- function(method) {
  # define environmental variable name
  env_var <- switch(method,
         'geocodio' = "GEOCODIO_API_KEY",
         'iq' = "LOCATIONIQ_API_KEY"
         )
  # load api key from environmental variable
  key <- Sys.getenv(env_var)
  
  if (key == "") stop(paste0("An API Key is needed to use the '", method, "' method.
    Set the \"", env_var, "\" variable in your .Renviron file or with Sys.getenv()."))
  else return(key)
}

# API URL Functions ----------------------------------------------------------------

# return : returntype => 'locations' or 'geographies'
# search:  searchtype => 'onelineaddress', 'addressbatch', 'address', or 'coordinates'
get_census_url <- function(return_type, search) {
  return(paste0("https://geocoding.geo.census.gov/geocoder/", return_type, "/", search))
}

get_geocodio_url <- function(api_v) {
  # return API URL based on api version (ex. 1.6)
  return(paste0("https://api.geocod.io/v", as.character(api_v), "/geocode"))
}

get_osm_url <- function() return("http://nominatim.openstreetmap.org/search")

get_iq_url <- function(region) {
  # region can be 'us' or 'eu'
  return(paste0("https://", region, "1.locationiq.com/v1/search.php"))
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
  
  # Throw error if user passes the same parameter via a custom api-specific list 
  # as they already did through the 'generic' parameters (address, street, etc.)
  for (custom_name in names(custom_parameters)) {
    if (custom_name %in% names(main_api_parameters)) {
      stop(paste0("Custom API Parameter '", custom_name, "' was already specified"))
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
  return( c(main_api_parameters, custom_parameters, default_api_parameters) )
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
#' @param address_list a list of addresses for batch geocoding (geocodio)
#' should be 'json' for geocodio and 'multipart' for census 
#' @param content_encoding Encoding to be used for parsing content
#' @param timeout timeout in minutes
#' @return raw results from the query
#' @examples
#' \donttest{
#' raw <- query_api("http://nominatim.openstreetmap.org/search", 
#'    get_api_query("osm", list(address = 'Hanoi, Vietnam')))
#'    
#' extract_results('osm', jsonlite::fromJSON(raw))
#' }
#' 
#' @seealso \code{\link{get_api_query}} \code{\link{extract_results}} \code{\link{geo}}
#' @export 
query_api <- function(api_url, query_parameters, mode = 'single', 
          batch_file = NULL, address_list = NULL, content_encoding = 'UTF-8', timeout = 20) {
   response <- switch(mode,
    'single' = httr::GET(api_url, query = query_parameters),
    'list' = httr::POST(api_url, query = query_parameters, 
          body = as.list(address_list), encode = 'json', httr::timeout(60 * timeout)),
    'file' = httr::POST(api_url,  
          body = c(list(addressFile = httr::upload_file(batch_file)), query_parameters),
          encode = 'multipart',
          httr::timeout(60*timeout))
   )
  
  httr::warn_for_status(response)
  content <- httr::content(response, as = 'text', encoding = content_encoding)
  return(content)
}

# Functions for displaying API queries (when verbose = TRUE) ----------------------

# print values in a named list (used for displaying query parameters)
display_named_list <- function(named_list) {
  for (var in names(named_list)) {
    ## censor API Key values
    if (var %in% c('api_key', 'key')) {
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
