
# Return a named list of a single parameter value to be passed to an API
# basd on the method and generic parameter name
get_default_param <- function(method_name, param_name) {
  return(
    tidygeocoder::api_parameter_reference %>% 
      dplyr::filter(method == method_name & generic_name == param_name) %>% 
      dplyr::select(api_name, default_value) %>% tibble::deframe(.)
  )
}

# Create parameter a value using a generic name
create_param <- function(method_name, param_name, value) {
  param <- list()
  param[[names(get_default_param(method_name,param_name))]] <- value
  return(param)
}

# Return the API URL for the specified method
get_api_url <- function(method_name,url_name=NULL) {
  tmp <- tidygeocoder::api_url_reference %>% 
    dplyr::filter(method == method_name) 
  
  if (is.null(url_name)) return(tmp %>% dplyr::slice(1) %>% dplyr::pull(api_url))
  else return(tmp %>% dplyr::filter(name == url_name) %>% dplyr::pull(api_url))
}

# Geocodio returns json by default
# Census geocoder is only api that does not offer the limit parameter to limit number of results returned


# Construct an an address api query
# api_key only needed for IQ and Geocodio services
############ NOTE: UNFINISHED ##########################################
### Fails on setting API Key
get_address_query <- function(method_name, address, api_key = NULL, limit = NULL) {
  
  required_fields_df <- tidygeocoder::api_parameter_reference %>% 
    dplyr::filter(method == method_name & required == TRUE)
  
  required_field_names <- required_fields_df %>% pull(generic_name)
  
  ## Set Address Parameter ----------------------------
  address_param <- create_param(method_name,'address',address)
  
  #print(address_param)
  
  ## Set API KEY Parameter ----------------------------
  if (('api_key' %in% required_field_names) & (!is.null(api_key))) {
    api_key_param <- create_param(method_name,'api_key',api_key)
  } else api_key_param <- list()
  
  # api_key_param <- dplyr::case_when(
  #   ('api_key' %in% required_field_names) ~ create_param(method_name,'api_key',api_key),
  #   TRUE ~ list()
  # )
  
  #print(api_key_param)
  
  # Extract default parameters 
  default_params <- tidygeocoder::api_parameter_reference %>% 
    dplyr::filter(method == method_name & required == TRUE) %>%
    dplyr::select(api_name, default_value) %>%
    filter(!is.na(default_value)) %>%
    tibble::deframe(.)
  
  # Combine address, api_key, and default parameters for full query
  return( c(address_param, api_key_param, default_params) )
}



## -----------------------------------------
  
# Create and Return Completely Custom Query OR build query in steps... (order shouldn't matter)
# 1. Add query defaults
# 2. Add address field
# 3. Add API Key (if necessary)
# 4. Set API URL

