#' Some sample addresses for testing
#'
#' @format A tibble dataframe with single line addresses
#' \describe{
#' \item{name}{Description of the address}
#' \item{addr}{Single line address}
#' }
#' @source NA
"sample_addresses"

#' Louisville, Kentucky street addresses
#'
#' @format A tibble dataframe with component street addresses
#' \describe{
#' \item{street}{Description of the address}
#' \item{city}{Single line address}
#' \item{state}{state}
#' \item{zip}{zip code}
#' }
#' @source Downloaded from http://results.openaddresses.io/sources/us/ky/jefferson
#'  on June 1st 2020
"louisville"

#' Geocoder Service REST API Parameter Reference
#' 
#' This dataset contains the mapping that allows this package to use a 
#' universal syntax for specifying parameters for different geocoder
#' service APIs. Reference the documentation for \code{\link{geo}} for
#' more information.
#'
#' @format A tibble dataframe
#' \describe{
#' \item{method}{Geocoder service name.}
#' \item{generic_name}{The universal name of the API parameter}
#' \item{api_name}{Name of the parameter for the specified geocoder service (method column value)}
#' \item{default_value}{Default value of this parameter}
#' \item{required}{Is the parameter required by the specified geocoder service}
#' }
#' 
#' @seealso \code{\link{get_api_query}} \code{\link{query_api}}
#' @source API Documentation:
#' \itemize{ 
#'     \item \href{https://locationiq.com/docs}{LocationIQ (IQ)} 
#'     \item \href{https://nominatim.org/release-docs/develop/api/Search}{Nominatim (OSM)}
#'     \item \href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census}
#'     \item \href{https://www.geocod.io/docs/}{Geocodio}
#' }     
"api_parameter_reference"