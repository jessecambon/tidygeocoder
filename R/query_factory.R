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
  
  # api_parameter_name <- tidygeocoder::api_parameter_reference %>% 
  #   dplyr::filter(method == method_name & generic_name == param_name) %>% 
  #   dplyr::pull(api_name) 
  
  # If api_parameter_name is NA or missing then return empty list
  if ((length(api_parameter_name) == 0)) return(list())
  if (is.na(api_parameter_name)) return(list())
  
  param <- list()
  param[[api_parameter_name]] <- value

  return(param)
}

# Return the API URL for the specified method
# if URL not found then return ""
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

# Construct an an api query based on generic parameters
# and optional api-specific parameters. Generic parameters
# are converted into api parameters using the api_parameter_reference
# dataset. API specific parameters can be provided directly with custom_api_parameters =
# Required defaults are filled in if not specified
get_api_query <- function(method_name, generic_parameters = list(), custom_api_parameters = list() ) {
  api_ref <- tidygeocoder::api_parameter_reference
  
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
#' @param content_encoding Encoding to be used for parsing content. 
#' @return raw results from the query
#' Census uses "ISO-8859-1", all other services use "UTF-8"
#' @export
query_api <- function(api_url, query_parameters, content_encoding='UTF-8') {
  response <- httr::GET(url = api_url, query = query_parameters)
  httr::warn_for_status(response)
  content <- jsonlite::fromJSON(httr::content(response, as = 'text', encoding = content_encoding))
  return(content)
}

