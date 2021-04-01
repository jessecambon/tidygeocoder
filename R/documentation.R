## Functions to produce documentation

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
  info <- as.list(api_info_reference[which(api_info_reference[['method']] == method) ,])
  
  bullet_intro <- paste0("- ", '`"', method, '"`: [', info[['method_display_name']], '](',
     info[['site_url']], ') geocoder service.')
  
  # if an API key is required list environmental variable
  api_requirements <- ifelse(
    method %in% api_key_reference[['method']],
    paste0(' An API key must be stored in "', 
           get_setting_value(api_key_reference, method, 'env_var'), '".'), 
  ''
  )
  
  # specify geographic limits if there are any.
  geographic_limits <- ifelse(method %in% names(pkg.globals$geographic_limitations),
    paste0(' Geographic coverage limited to the ', pkg.globals$geographic_limitations[[method]], '.'), 
    ''
  )

  # note if batch geocoding needs to be manually specified
  batch_capabilities <- if (method %in% batch_limit_reference[['method']]) {
    ifelse(method %in% names(pkg.globals$single_first_methods), 
        ' Batch geocoding is supported, but must be explicitly called with `mode = "batch"`.',
        ' Batch geocoding is supported.'
    )
  } else ''

  return(paste0(bullet_intro, geographic_limits, api_requirements, batch_capabilities))
}


# Returns methods documentation
# reverse = TRUE if reverse geocoding, FALSE if forward geocoding
get_method_documentation <- function(reverse) {
  all_methods <- api_info_reference[['method']]
  
  method_intro <- c(
    "the geocoder service to be used. Refer to [api_parameter_reference]",
    "and the API documentation for each geocoder service for usage details and limitations.",
    "Run `usethis::edit_r_environ()` to open your .Renviron file for editing to add API keys",
    "as environmental variables.")
  
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