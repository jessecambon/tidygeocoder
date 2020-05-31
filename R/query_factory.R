#############################################
##### Functions for constructing API queries
#############################################

##### API URLS -----------------------------------------------------

# Return the API URL for the specified method
# if URL not found then return ""
#### TODO - incorporate flexible census url
get_api_url <- function(method_name,url_name=NULL) {
  # select rows pertaining to the relevant method
  url_ref <- tidygeocoder::api_url_reference
  
  tmp <- url_ref[which(url_ref['method'] == method_name),]
  
  if (nrow(tmp) == 0) return('')
  
  # Select the first relevant listed api_url if url_name is NULL
  # Otherwise select the url_name specified
  if (is.null(url_name)) selected_url <- tmp[[1,'api_url']]
  else selected_url <- tmp[which(tmp['name'] == url_name),'api_url'][[1]]
  
  if (length(selected_url) == 0) return('')
  else return(selected_url)
}

# Get API Key from environmental variables
get_key <- function(method) {
  # define environmental variable name
  env_var <- switch(method,
         'geocodio' = "GEOCODIO_API_KEY",
         'iq' = "LOCATIONIQ_API_KEY"
         )
  # load api key from environmental variable
  key <- Sys.getenv(env_var)
  
  if (key == "") {
    warning(paste0("An API Key is needed to use the '", method, "' method.\n
    Set the \"", env_var, "\" variable in your .Renviron file."))
  }
  return(key)
}

### Get API URL for Census geocoder
# return : returntype => 'locations' or 'geographies'
# search:  searchtype => 'onelineaddress', 'addressbatch', 'address', or 'coordinates'
get_census_url <- function(return, search) {
  return(paste0("https://geocoding.geo.census.gov/geocoder/", return, "/", search))
}


##### API PARAMETERS -------------------------------------------------

# Create an API-specific parameter for a given method
# given generic parameter name and a value
create_api_parameter <- function(method_name, param_name, value) {
  api_ref <- tidygeocoder::api_parameter_reference
  
  # Extract the API specific parameter name
  api_parameter_name <- 
    api_ref[which( (api_ref$method == method_name) &
        (api_ref$generic_name ==  param_name)),'api_name'][[1]]
  #print('api_parameter_name:')
  #print(api_parameter_name)
  
  # If api_parameter_name is NA or missing then return empty list
  if ((length(api_parameter_name) == 0)) return(list())
  if (is.na(api_parameter_name)) return(list())
  
  param <- list()
  param[[api_parameter_name]] <- value
  return(param)
}

# Construct an an api query based on generic parameters
# and optional api-specific parameters. Generic parameters
# are converted into api parameters using the api_parameter_reference
# dataset. API specific parameters can be provided directly with custom_api_parameters =
# Required defaults are filled in if not specified
#' Function for creating api queries
#'
#' @export
get_api_query <- function(method_name, generic_parameters = list(), custom_api_parameters = list() ) {
  api_ref <- tidygeocoder::api_parameter_reference
  
  # create the "main" api parameters from the passed generic parameters
  main_api_parameters <- list()
  for (generic_parameter_name in names(generic_parameters)) {
    main_api_parameters <- c(main_api_parameters, 
      create_api_parameter(method_name, generic_parameter_name,
      generic_parameters[[generic_parameter_name]])
      )
  }
  #### TODO ----- Check for overlap between generic_parameters and custom_api_parameters
  
  #print('main_api_parameters: ')
  #print(main_api_parameters)
  
  ## Extract default parameter values ---------------
  # only extract values that have default values and don't already exist in main_api_parameters
  default_api_parameters <- tibble::deframe(
    api_ref[which(api_ref$method == method_name &
      api_ref$required == TRUE &
      !is.na(api_ref$default_value) &
      !api_ref$api_name %in% names(c(main_api_parameters,custom_api_parameters))),][c('api_name','default_value')]
    )
  
  #print('default_api_parameters:')
  #print(default_api_parameters)
  
  # Combine address, api_key, and default parameters for full query
  return( c(main_api_parameters, custom_api_parameters, default_api_parameters) )
}

#' Get raw results from an API
#' @param api_url Base URL of the API. query parameters are appended to this
#' @param query_parameters api query parameters in the form of a named list
#' @param mode Values:
#'     "single" - geocode a single address
#'     "list" - batch geocode list of addresses (geocodio)
#'     "file" - batch geocode a file of addresses (census)
#' @param batch_file a csv file of addresses to upload (census)
#' @param address_list a list of addresses for batch geocoding (geocodio)
#' should be 'json' for geocodio and 'multipart' for census 
#' @param content_encoding Encoding to be used for parsing content. 
#' @param timeout timeout in minutes for batch geocoding
#' @return raw results from the query
#' Census uses "ISO-8859-1", all other services use "UTF-8"
#' @export
query_api <- function(api_url, query_parameters, mode = 'single', 
          batch_file=NULL, address_list = NULL, content_encoding='UTF-8', timeout = 15) {
   response <- switch(mode,
    'single' = httr::GET(api_url, query = query_parameters),
    'list' = httr::POST(api_url, query = query_parameters, 
          body = as.list(address_list), encode = 'json', httr::timeout(60*timeout)),
    'file' = httr::POST(api_url,  
          body = c(list(addressFile = httr::upload_file(batch_file)), query_parameters),
          encode = 'multipart',
          httr::timeout(60*timeout))
   )
  
  httr::warn_for_status(response)
  content <- httr::content(response, as = 'text', encoding = content_encoding)
  return(content)
}

