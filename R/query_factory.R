# Create an API-specific parameter for a given method
# given generic parameter name and a value
create_api_parameter <- function(method_name, param_name, value) {
  
  # Find the API specific parameter name
  api_parameter_name_df <- 
    tidygeocoder::api_parameter_reference[(tidygeocoder::api_parameter_reference['method'] == method_name) &
    tidygeocoder::api_parameter_reference['generic_name'] ==  param_name,'api_name'] 
  
  # If api_parameter_name not found then return empty list
  if (nrow(api_parameter_name_df) != 1) return(list())
  
  # Extract character value
  api_parameter_name <- as.character(api_parameter_name_df)
  
  # api_parameter_name <- tidygeocoder::api_parameter_reference %>% 
  #   dplyr::filter(method == method_name & generic_name == param_name) %>% 
  #   dplyr::pull(api_name) 
  
  # If api_parameter_name is NA or missing then return empty list
  #if ((length(api_parameter_name) == 0)) return(list())
  if (is.na(api_parameter_name)) return(list())
  
  param <- list()
  param[[api_parameter_name]] <- value

  return(param)
}

# Return the API URL for the specified method
get_api_url <- function(method_name,url_name=NULL) {
  # select rows pertaining to the relevant method
  tmp <- tidygeocoder::api_url_reference[tidygeocoder::api_url_reference['method'] == method_name,]
  
  # Select the first relevant listed api_url if url_name is NULL
  # Otherwise select the url_name specified
  if (is.null(url_name)) selected_url <- tmp[1,'api_url']
  else selected_url <- tmp[tmp['name'] == url_name,'api_url']

  if (nrow(selected_url) != 1) stop('API URL not found')
  else return(as.character(selected_url))
}

# Geocodio returns json by default
# Census geocoder is only api that does not offer the limit parameter to limit number of results returned


# Construct an an api query based on generic parameters
# and optional api-specific parameters. Generic parameters
# are converted into api parameters using the api_parameter_reference
# dataset

# api_key only needed for IQ and Geocodio services
get_api_query <- function(method_name, generic_parameters, custom_api_parameters = list() ) {
  
  # required_fields_df <- tidygeocoder::api_parameter_reference %>% 
  #   dplyr::filter(method == method_name & required == TRUE)
  # required_field_names <- required_fields_df %>% pull(generic_name)
  
  # create the "main" api parameters from the passed generic parameters
  main_api_parameters <- list()
  for (generic_parameter_name in names(generic_parameters)) {
    main_api_parameters <- c(main_api_parameters, 
      create_api_parameter(method_name, generic_parameter_name, 
      generic_parameters[[generic_parameter_name]]) 
      )
  }
  
  ## Extract default parameter values ---------------
  default_api_parameters <- tidygeocoder::api_parameter_reference %>% 
    dplyr::filter(method == method_name & required == TRUE) %>%
    dplyr::select(api_name, default_value) %>%
    # only extract values that have default values and don't already exist in main_api_parameters
    filter(!is.na(default_value) & (!api_name %in% names(main_api_parameters))) %>%
    tibble::deframe(.)
  
  # Combine address, api_key, and default parameters for full query
  return( c(main_api_parameters,default_api_parameters) )
}

#' Get raw results back from an API
#' @param api_url Base URL of the API. query parameters are appended to this
#' @param query_parameters api query parameters in the form of a named list
#' @param content_encoding Encoding to be used for parsing content. 
#' @return raw results from the query
#' Census uses "ISO-8859-1", all other services use "UTF-8"
#' @export
query_api <- function(api_url, query_parameters, content_encoding='UTF-8') {
  response <- httr::GET(url = api_url, query = query_parameters)
  return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = content_encoding))
  )
}


## -----------------------------------------
  
# Create and Return Completely Custom Query OR build query in steps... (order shouldn't matter)
# 1. Add query defaults
# 2. Add address field
# 3. Add API Key (if necessary)
# 4. Set API URL

##### SCRAP -------------------------------

## Set Address Parameter ----------------------------
# address_param <- create_api_parameter(method_name,'address',address)

#print(address_param)

## Set API KEY Parameter ----------------------------
# if ('api_key' %in% required_field_names) {
#   if (is.null(api_key)) {
#     warning('Required parameter API Key not specified')
#     return(list())
#   } else {
#     api_key_param <- create_api_parameter(method_name,'api_key',api_key)
#   }
# } else {
#   # if api key isn't required then leave it blank
#   api_key_param <- list()
# }

## Set limit Parameter ----------------------------
# if (is.null(limit)) limit_param <- list()
# else limit_param <- limit_param <- create_api_parameter(method_name,'limit',limit)

