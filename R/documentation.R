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


# Get bullets for methods documentation
# reverse = TRUE if reverse geocoding, FALSE if forward geocoding
get_method_bullet_list <- function(reverse) {
  all_methods <- api_info_reference[['method']]
  
  # if reverse geocoding then exclude methods that don't support reverse geocoding
  methods_to_list <- if (reverse == TRUE) {
    setdiff(all_methods, pkg.globals$no_reverse_methods)
  } else all_methods
  
  return(
    sapply(methods_to_list, get_method_bullet, USE.NAMES = FALSE)
  )
}


#' 
#' get_method_bullet_list <- function() {
#'   
#'   @param method the geocoder service to be used. Refer to 
#'   [api_parameter_reference] and the API documentation for
#'   each geocoder service for usage details and limitations. Run `usethis::edit_r_environ()`
#'    to open your .Renviron file for editing to add API keys as an environmental variables.
#'    - `"census"`: US Census Geocoder. US street-level addresses only. 
#'       Can perform batch geocoding.
#'    - `"osm"`: Nominatim (OSM). Worldwide coverage.
#'    - `"geocodio"`: Commercial geocoder. Covers US and Canada and has
#'       batch geocoding capabilities. Requires an API Key to be stored in
#'       the "GEOCODIO_API_KEY" environmental variable.
#'    - `"iq"`: Commercial Nominatim geocoder service. Requires an API Key to
#'       be stored in the "LOCATIONIQ_API_KEY" environmental variable.
#'    - `"google"`: Commercial Google geocoder service. Requires an API Key to
#'       be stored in the "GOOGLEGEOCODE_API_KEY" environmental variable.
#'    - `"opencage"`: Requires an API Key to be stored
#'       in the "OPENCAGE_KEY" environmental variable.
#'    - `"mapbox"`: Commercial Mapbox geocoder service. Requires an API Key to
#'       be stored in the "MAPBOX_API_KEY" environmental variable.
#'    - `"here"`: Commercial HERE geocoder service. Requires an API Key 
#'       to be stored in the "HERE_API_KEY" environmental variable. Can perform 
#'       batch geocoding, but this must be specified with `mode = 'batch'`.
#'    - `"tomtom"`: Commercial TomTom geocoder service. Requires an API Key to
#'       be stored in the "TOMTOM_API_KEY" environmental variable. Can perform 
#'       batch geocoding.
#'    - `"mapquest"`: Commercial MapQuest geocoder service. Requires an 
#'       API Key to be stored in the "MAPQUEST_API_KEY" environmental variable. 
#'       Can perform batch geocoding.
#'    - `"bing"`: Commercial Bing geocoder service. Requires an 
#'       API Key to be stored in the "BINGMAPS_API_KEY" environmental variable. 
#'       Can perform batch geocoding.
#'    - `"arcgis"`: Commercial ArcGIS geocoder service.
#'    - `"cascade"` : Attempts to use one geocoder service and then uses
#'      a second geocoder service if the first service didn't return results.
#'      The services and order is specified by the cascade_order argument. 
#'      Note that this is not compatible with `full_results = TRUE} as geocoder
#'      services have different columns that they return.
#' }