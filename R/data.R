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
#' Note that latitude and longitude input parameters for reverse geocoding
#' are not in this dataset and are instead handled directly by the \code{reverse_geo} function.
#' 
#' The \code{generic_name} column is a universal parameter name that is shared between services.
#' The \code{api_name} column is the parameter name for the given geocoder service specified by the
#'\code{method} column. When \code{generic_name} is missing 
#' this means the parameter is specific to that geocoder service.
#' 
#' While the "census" and "google" services do not have a \code{limit}
#' argument in their APIs, tidygeocoder provides a passthrough so you can still
#' use the \code{limit} argument in \code{geo} and \code{reverse_geo} to limit the 
#' number of result per input.
#' 
#' Note that some geocoder services only use the \code{limit} argument for forward geocoding.
#' Refer to API documentation of each service for more information.
#'  
#' Reference the documentation for \code{\link{geo}} and \code{\link{reverse_geo}}  for more information. 
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
#' @details 
#' 
#' The API documentation for each service is linked to below:
#' 
#' @includeRmd external/api_documentation_urls.md
#' 
#' @seealso \code{\link{get_api_query}} \code{\link{query_api}} \code{\link{geo}} \code{\link{geocode}}
#' 
#' 
"api_parameter_reference"

#' The batch limit for each geocoder service
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{batch_limit}{The maximum number of addresses or coordinates allowed per batch}
#' }
"batch_limit_reference"

#' The minimum number of seconds required per query to comply with usage restrictions. 
#' The \code{\link{geo}} and \code{\link{reverse_geo}} functions use this value to
#' slow down the rate of querying if necessary.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{min_time}{The minimum number of seconds required per query to comply with usage restrictions}
#'  \item{description}{A description of the usage rate restriction}
#' }
#' @details 
#' 
#' Links to the usage policies of each geocoder service is below:
#' 
#' @includeRmd external/api_usage_urls.md
"min_time_reference"

#' The name of the environmental variable that the API key will be read from.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{env_var}{Environmental variable name}
#' }
"api_key_reference"