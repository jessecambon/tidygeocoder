#' Convenience functions for calling `geo()`
#' 
#' @description The `method` for `geo()` is specified in the function name.
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' Use the [geo] function directly instead.
#' 
#' @export
#' @param ... arguments to be passed to the `geo` function
geo_census <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_census()", "geo()")
  return(geo(method = 'census', ...))
}
  
#'@rdname geo_census
#' @export
geo_osm <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_osm()", "geo()")
  return(geo(method = 'osm', ...))
}

#'@rdname geo_census
#' @export
geo_geocodio <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_geocodio()", "geo()")
  return(geo(method = 'geocodio', ...))
}

#'@rdname geo_census
#' @export
geo_iq <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_iq()", "geo()")
  return(geo(method = 'iq', ...))
}

#'@rdname geo_census
#' @export
geo_google <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_google()", "geo()")
  return(geo(method = 'google', ...))
}

#'@rdname geo_census
#' @export
geo_opencage <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_opencage()", "geo()")
  geo(method = 'opencage', ...)
}

#'@rdname geo_census
#' @export
geo_mapbox <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_mapbox()", "geo()")
  return(geo(method = 'mapbox', ...))
}

#'@rdname geo_census
#' @export
geo_here <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_here()", "geo()")
  return(geo(method = 'here', ...))
}

#'@rdname geo_census
#' @export
geo_tomtom <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_tomtom()", "geo()")
  return(geo(method = 'tomtom', ...))
}

#'@rdname geo_census
#' @export
geo_mapquest <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_mapquest()", "geo()")
  return(geo(method = 'mapquest', ...))
}

#'@rdname geo_census
#' @export
geo_bing <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_bing()", "geo()")
  return(geo(method = 'bing', ...))
}

#'@rdname geo_census
#' @export
geo_arcgis <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_arcgis()", "geo()")
  return(geo(method = 'arcgis', ...))
}

#' @rdname geo_census
#' @export
geo_cascade <- function(...) {
  lifecycle::deprecate_warn("1.0.3", "geo_cascade()", "geo()")
  return(geo(method = 'cascade', ...))
}




## New private cascade geocoding function
## called from geo()
cascade_geocoding <- function(..., cascade_order = c('census', 'osm')) {
  # Note for both attempts we disable unique_only so as to keep the 
  # input address lists in line with the dataframe output so we can
  # more easily splice the results together
  input <- list(...)
  
  if ('lat' %in% names(input)) lat <- input[['lat']]
  else lat <- 'lat'
  if ('long' %in% names(input)) long <- input[['long']]
  else long <- 'long'
  
  # if the input is an unnamed list then pass it directly, else remove certain named elements
  # this is necessary because names(input) returns NULL if input is an unnamed list
  if (is.null(names(input))) {
    initial_parameters <- input
    unique_results_requested <- FALSE
  }
  else {
    initial_parameters <- input[!names(input) %in% c('method', 'unique_only', 'cascade_order')]
    unique_results_requested <- ifelse('unique_only' %in% names(input), input[['unique_only']], FALSE)
  }
  initial_parameters <- c(initial_parameters, list(method = cascade_order[1], unique_only = FALSE))
  
  # first attempt 
  initial_results = do.call(geo, initial_parameters)
  
  # find NA result indices for initial attempt
  na_indices <- is.na(initial_results[[lat]]) | is.na(initial_results[[long]])
  
  if (any(na_indices)) {
    # if some results weren't found then we retry
    if (is.null(names(initial_parameters))) retry_addresses <- input[na_indices]
    else retry_addresses <- lapply(input[names(input) %in% pkg.globals$address_arg_names], 
                                   function(x, bools) x[bools], na_indices)
    
    retry_results = do.call(geo, c(retry_addresses,
                                   input[!names(input) %in% c(pkg.globals$address_arg_names, 'method', 'unique_only')],
                                   list(method = cascade_order[2], unique_only = FALSE)))
    
    ## combine initial and retry results
    combi <- initial_results
    combi[na_indices,] <- retry_results
    combi$geo_method <- ifelse(na_indices, cascade_order[2], cascade_order[1])
  } else {
    # if all addresses were found then just return initial results
    combi <- initial_results
    combi$geo_method <- cascade_order[1]
  }
  
  # if user requested unique only then return distinct dataset
  if (unique_results_requested == TRUE) return(unique(combi)) 
  else return(combi)
}
