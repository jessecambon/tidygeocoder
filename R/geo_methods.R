#' Conveniance functions for calling the \code{\link{geo}} function
#' with a specified method.
#' 
#' @export
geo_census <- function(...) geo(method = 'census', ...)
  
#'@rdname geo_census
#' @export
geo_osm <- function(...) geo(method = 'osm',...)