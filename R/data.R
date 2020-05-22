#' Some sample addresses for testing
#'
#' @format A tibble dataframe with single line addresses
#' \describe{
#' \item{name}{Description of the address}
#' \item{addr}{Single line address}
#' }
#' @source NA
"sample_addresses"

#' REST API Parameter Reference
#'
#' @format A tibble dataframe
#' \describe{
#'   ~method,  ~generic_name,  ~api_name,   ~default_value,   ~required,
#' \item{method}{Geocoder service shortname}
#' \item{generic_name}{Generic name of the API parameter}
#' \item{api_name}{Name of the parameter for the geocoder service specified in the method field}
#' \item{default_value}{Default value of this parameter}
#' \item{required}{Is the parameter required by the API?}
#' 
#' }
#' @source \itemize{ 
#'     \item \href{https://locationiq.com/docs}{IQ API Reference} 
#'     \item \href{https://nominatim.org/release-docs/develop/api/Search}{Nominatim/OSM API Reference}
#'     \item \href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census API Documentation}
#'     
"api_parameter_reference"