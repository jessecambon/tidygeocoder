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
#' 
#' The \code{generic_name} field is a universal field name while the \code{api_name}
#' field shows the specific parameter name for the given geocoder service (\code{method}).
#' When the \code{api_name} is missing this means that the parameter is not 
#' supported by the given geocoder service. When \code{generic_name} is missing 
#' this means the parameter is specific to that geocoding service.
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
#' }     
"api_parameter_reference"