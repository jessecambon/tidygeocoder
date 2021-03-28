#' Some sample addresses for testing
#'
#' @format A tibble dataframe with single line addresses
#' \describe{
#'  \item{name}{Description of the address}
#'  \item{addr}{Single line address}
#' }
"sample_addresses"

#' Louisville, Kentucky street addresses
#'
#' @format A tibble dataframe with component street addresses
#' \describe{
#'  \item{street}{Description of the address}
#'  \item{city}{Single line address}
#'  \item{state}{state}
#'  \item{zip}{zip code}
#' }
#' @source Downloaded from \href{http://results.openaddresses.io/sources/us/ky/jefferson}{OpenAddresses.io}
#'  on June 1st 2020
"louisville"

#' Geocoder service API parameter reference
#' 
#' @description
#' This dataset contains the mapping that allows this package to use a 
#' universal syntax to specify parameters for different geocoder services. 
#' Note that latitude nad longitude input parameters for reverse geocoding
#' are not in this dataset and are instead handled directly by the \code{reverse_geo} function.
#' 
#' The \code{generic_name} column is a universal parameter name that is shared between services.
#' The \code{api_name} column is the parameter name for the given geocoder service specified by the
#'\code{method} column. When \code{generic_name} is missing 
#' this means the parameter is specific to that geocoder service.
#' 
#' Note that while the "census" and "google" services do not have a \code{limit}
#' argument in their APIs, tidygeocoder provides a passthrough so you can still
#' use the \code{limit} argument in \code{geo} and \code{reverse_geo} to limit the 
#' number of result per input.
#'  
#' Reference the documentation for \code{\link{geo}} for more information. 
#' Also reference \code{vignette("tidygeocoder")} for more details on constructing API queries.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{generic_name}{Universal parameter name}
#'  \item{api_name}{Name of the parameter for the specified geocoder service}
#'  \item{default_value}{Default value of the parameter}
#'  \item{required}{Is the parameter required by the specified geocoder service?}
#' }
#' 
#' @seealso \code{\link{get_api_query}} \code{\link{query_api}} \code{\link{geo}} \code{\link{geocode}}
#' @source Links to API documentation for each geocoder service are below.
#' \itemize{ 
#'  \item \href{https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html}{Census}
#'  \item \href{https://nominatim.org/release-docs/develop/api/Search/}{Nominatim} ("osm")
#'  \item \href{https://www.geocod.io/docs/}{Geocodio}
#'  \item \href{https://locationiq.com/docs}{Location IQ} ("iq") 
#'  \item \href{https://developers.google.com/maps/documentation/geocoding/overview}{Google}
#'  \item \href{https://opencagedata.com/api}{OpenCage}
#'  \item \href{https://docs.mapbox.com/api/search/geocoding/}{Mapbox}
#'  \item \href{https://developer.here.com/documentation/geocoding-search-api/dev_guide/index.html}{HERE}
#'  \item \href{https://developer.tomtom.com/search-api/search-api-documentation-geocoding/geocode}{TomTom}
#'  \item \href{https://developer.mapquest.com/documentation/geocoding-api/}{MapQuest}
#'  \item \href{https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/}{Bing}
#'  \item \href{https://developers.arcgis.com/rest/geocode/api-reference/overview-world-geocoding-service.htm}{ArcGIS}
#' }     
"api_parameter_reference"

#' The batch limit for each geocoder service
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{batch_limit}{The maximum number of addresses or coordinates allowed per batch}
#' }
"batch_limit_reference"

#' The minimum number of seconds required per query to comply with usage restrictions
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{min_time}{The minimum number of seconds required per query to comply with usage restrictions}
#'  \item{description}{A description of the usage rate restriction}
#' }
"min_time_reference"

#' The name of the environmental variable where the API key will be read from.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{env_var}{Environmental variable name}
#' }
"api_key_reference"