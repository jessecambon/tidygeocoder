### Functions to produce documentation
# Functions return a character vector with one element per line
# to produce roxygen documention
# see: https://roxygen2.r-lib.org/articles/rd-formatting.html#dynamic-r-code 


# Utility functions ---------------------------------------------------------------

# create a string list given a character vector input
# ie. c('eggs', 'cheese', 'ham')  ->  eggs, cheese, and ham
# optional wrap argument can be used to add quotations
# ie create_comma_list(c('eggs', 'cheese'), wrap = '"')  ->  "eggs" and "cheese"

create_comma_list <- function(v, wrap = "") {
  # wrap each element in quotes
  if (wrap != "") {
    v <- sapply(v, function(x) paste0(c(wrap, x, wrap), collapse = ''), USE.NAMES = FALSE)
  }
  
  if (length(v) <= 1) return(v)
  else if (length(v) == 2) return(paste0(v, collapse = ' and '))
  else if (length(v) > 2) {
    ending <- paste0(v[(length(v) - 1)], ', and ', v[length(v)], collapse = '')
    beginning <- paste0(v[1 : (length(v) - 2)], collapse = ', ')
    return(
      paste0(c(beginning, ending), collapse = ', ')
    )
  }
}

# get replaceable coordinate/address terms
get_coord_address_terms <- function(reverse) {
  terms <- list()
  if (reverse == TRUE) {
    terms$input_singular <- 'coordinate'
    terms$input_plural <- 'coordinates'
    terms$return_arg <- "return_coords"
  } else {
    terms$input_singular <- 'address'
    terms$input_plural <- 'addresses'
    terms$return_arg <- "return_addresses"
  }
  return(terms)
}

# Documentation functions ---------------------------------------------------------------


# produce markdown list of api documentation urls
get_api_doc_bullets <- function() {
  return(
    paste0(
          '- [', tidygeocoder::api_info_reference$method_display_name,
          '](', tidygeocoder::api_info_reference$api_documentation_url, ')'
    )
  )
}

# produce markdown list of api usage policy urls
get_api_usage_bullets <- function() {
  return(
    paste0(
      '- [', tidygeocoder::api_info_reference$method_display_name,
      '](', tidygeocoder::api_info_reference$api_usage_policy_url, ')'
    )
  )
}


get_method_bullet <- function(method) {
  # extract row in api_info_reference and convert to list
  info <- as.list(tidygeocoder::api_info_reference[which(tidygeocoder::api_info_reference[['method']] == method) ,])
  
  bullet_intro <- paste0("- ", '`"', method, '"`: [', info[['method_display_name']], '](',
     info[['site_url']], ').')
  
  # if an API key is required list environmental variable
  api_requirements <- ifelse(
    method %in% tidygeocoder::api_key_reference[['method']],
    paste0('An API key must be stored in "', 
           get_setting_value(tidygeocoder::api_key_reference, method, 'env_var'), '".'), 
  ''
  )
  
  # specify geographic limits if there are any.
  geographic_limits <- ifelse(method %in% names(pkg.globals$geographic_limitations),
    paste0('Geographic coverage is limited to the ', pkg.globals$geographic_limitations[[method]], '.'), 
    ''
  )

  # note if batch geocoding needs to be manually specified
  batch_capabilities <- if (method %in% tidygeocoder::batch_limit_reference[['method']]) {
    ifelse(method %in% pkg.globals$single_first_methods, 
        'Batch geocoding is supported, but must be explicitly called with `mode = "batch"`.',
        'Batch geocoding is supported.'
    )
  } else ''

  return(paste0(c(bullet_intro, geographic_limits, api_requirements, batch_capabilities), collapse = ' '))
}


# Returns methods documentation
# reverse = TRUE if reverse geocoding, FALSE if forward geocoding
get_method_documentation <- function(reverse) {
  all_methods <- tidygeocoder::api_info_reference[['method']]
  
  method_intro <- paste0(c(
    "the geocoder service to be used.", 
    "API keys are loaded from environmental variables. Run `usethis::edit_r_environ()` to open",
    'your .Renviron file and add an API key as an environmental variable. For example, add the line `GEOCODIO_API_KEY="YourAPIKeyHere"`.'
  ), collapse = ' ')
  
  # if reverse geocoding then exclude methods that don't support reverse geocoding
  methods_to_list <- if (reverse == TRUE) {
    setdiff(all_methods, pkg.globals$no_reverse_methods)
  } else all_methods
  
  return(
    c(
      method_intro,
      sapply(methods_to_list, get_method_bullet, USE.NAMES = FALSE)
    )
  )
}

get_batch_limit_documentation <- function(reverse) {
  terms <- get_coord_address_terms(reverse)
  return(
    c(
      paste0(c("limit to the number of ", terms$input_plural, " in a batch geocoding query."), collapse = ''),
      "Defaults to the value in [batch_limit_reference] if not specified."
    )
  )
}

get_batch_limit_error_documentation <- function(reverse) {
  terms <- get_coord_address_terms(reverse)
  cascade_message <- if (reverse == FALSE) " This is reverted to FALSE when using the cascade method." else ""
  
  return(
    c(
      paste0(c("if TRUE then an error is thrown if the number of ", terms$input_plural, " exceeds the batch limit."), collapse = ''),
      paste0(c("(if executing a batch query).", cascade_message), collapse = '')
    )
  )
}

get_limit_documentation <- function(reverse, df_input) {
  terms <- get_coord_address_terms(reverse)

  main <- c(
    paste0(c("maximum number of results to return per input ", terms$input_singular, ". For many geocoder services"), collapse = ''),
    "the maximum value of the limit parameter is 100. Pass `limit = NULL` to use",
    "the default `limit` value of the selected geocoder service.",
    paste0(c("For batch geocoding, limit must be set to 1 (default) if `", terms$return_arg, " = TRUE`."), collapse = '')
  )
  
  append <- if (df_input == FALSE) c() else {
    paste0(c("To use `limit > 1` or `limit = NULL` set return_input to FALSE."), collapse = '')
  }
  
  return(c(main, append, "Refer to [api_parameter_reference] for more details."))
}

get_mode_documentation <- function(reverse) {
  terms <- get_coord_address_terms(reverse)
  
  return(
    c(
      paste0(c("set to 'batch' to force batch geocoding or 'single' to force single ", terms$input_singular), collapse = ''),
      paste0(c("geocoding (one ", terms$input_singular, " per query). If not specified then batch geocoding will"), collapse = ''),
      paste0(c("be used if available (given method selected) when multiple ", terms$input_plural, " are"), collapse = ''),
      paste0(c("provided; otherwise single address geocoding will be used. For the ", 
            create_comma_list(pkg.globals$single_first_methods, wrap = '"'), " methods the"), collapse = ''),
      "batch mode should be explicitly specified with `mode = 'batch'`."
      )
  )
}