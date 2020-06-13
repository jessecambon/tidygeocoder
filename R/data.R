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

#' REST API Parameter Reference
#'
#' @format A tibble dataframe
#' \describe{
#' \item{method}{Geocoder service shortname}
#' \item{generic_name}{Generic name of the API parameter}
#' \item{api_name}{Name of the parameter for the geocoder service specified in the method field}
#' \item{default_value}{Default value of this parameter}
#' \item{required}{Is the parameter required by the API?}
#' }
#' @source \itemize{ 
#'     \item \href{https://locationiq.com/docs}{LocationIQ (IQ)} 
#'     \item \href{https://nominatim.org/release-docs/develop/api/Search}{Nominatim (OSM)}
#'     \item \href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census}
#'     \item \href{https://www.geocod.io/docs/}{Geocodio}
#' }     
"api_parameter_reference"