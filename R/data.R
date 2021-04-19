#' Sample addresses for testing
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
#' @source Downloaded from [OpenAddresses.io](https://results.openaddresses.io/sources/us/ky/jefferson)
#'  on June 1st 2020
"louisville"

#' Geocoder service API parameter reference
#' 
#' @description
#' This dataset contains the mapping that allows this package to use a 
#' universal syntax to specify parameters for different geocoder services. 
#' Note that latitude and longitude input parameters for reverse geocoding
#' are not in this dataset and are instead handled directly by the [reverse_geo] function.
#' 
#' The `generic_name` column is a universal parameter name that is shared between services.
#' The `api_name` column is the parameter name for the given geocoder service specified by the
#' `method` column. When `generic_name` is missing 
#' this means the parameter is specific to that geocoder service.
#' 
#' While the `r create_comma_list(pkg.globals$limit_passthru_methods, wrap ='"')` services do not have a `limit`
#' argument in their APIs, tidygeocoder provides a passthrough so you can still
#' use the `limit` argument in [geo] and [reverse_geo] to limit the 
#' number of results per input.
#' 
#' Note that some geocoder services only use the `limit` argument for forward geocoding.
#' Refer to API documentation of each service for more information.
#'  
#' Reference the documentation for [geo] and [reverse_geo] for more information. 
#' Also reference `vignette("tidygeocoder")` for more details on constructing API queries.
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
#' `r get_api_doc_bullets()`
#' 
#' @seealso [geo] [reverse_geo] [get_api_query] [query_api] [min_time_reference] [batch_limit_reference]
#' 
"api_parameter_reference"

#' Geocoding batch size limits
#' 
#' The [geo] and [reverse_geo] functions use this dataset to set the
#' maximum batch query size for each service.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{batch_limit}{The maximum number of addresses or coordinates allowed per batch}
#' }
#' @seealso [geo] [reverse_geo] 
#' 
"batch_limit_reference"

#' Minimum time required per query
#' 
#' The [geo] and [reverse_geo] functions use this dataset
#' to set the maximum query rate for each geocoder service.
#' This rate is based on the usage restriction policies for 
#' each geocoder service.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{min_time}{The minimum number of seconds required per query to comply with usage restrictions}
#'  \item{description}{A description of the usage rate restriction}
#' }
#' @details Links to the usage policies of each geocoder service are below:
#' 
#' `r get_api_usage_bullets()`
#' 
#' @seealso [geo] [reverse_geo]
#' 
"min_time_reference"

#' API key environmental variables
#' 
#' API keys are obtained from environmental variables.
#' The [geo] and [reverse_geo] functions use this dataset
#' to know which environmental variable to use for
#' each geocoder service.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{env_var}{Environmental variable name}
#' }
#' 
#' @seealso [geo] [reverse_geo]
#' 
"api_key_reference"

#' Geocoder service links and information
#' 
#' This dataset is used for generating package documentation.
#'
#' @format A tibble dataframe
#' \describe{
#'  \item{method}{Geocoder service name}
#'  \item{method_display_name}{Geocoder service display name}
#'  \item{site_url}{Link to the main site of the geocoder service}
#'  \item{api_documentation_url}{Link to API documentation}
#'  \item{api_usage_policy_url}{Link to the usage policy}
#' }
#' 
"api_info_reference"