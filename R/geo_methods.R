#' Conveniance functions for calling the \code{\link{geo}} function
#' with a specified method.
#' 
#' @export
geo_census <- function(...) geo(method = 'census', ...)
  
#'@rdname geo_census
#' @export
geo_osm <- function(...) geo(method = 'osm',...)

#' First try census then try osm. Considering deprecating...
#' NOTE ----- UNTESTED
#' Title
#' @rdname geo_census
#' @export
geo_cascade <- function(...) {
  
  # first attempt census
  census_results = geo(method = 'census',...)
  
  # if census results are NA then attempt osm
  if (is.na(census_results[[1,1]]) | is.na(census_results[[1,2]])) {
    osm_results = geo(method = 'osm', ...)
    if (is.na(osm_results[[1,1]]) | is.na(osm_results[[1,2]])) {
      return(osm_results)
    } else {
      osm_results$geo_method = 'osm'
    }
    return(osm_results)
  } else {
    # If the census results are NOT NA ...
    census_results$geo_method = 'census'
    return(census_results)
  }
}