#' Geocode addresses in a dataframe
#'
#' @description
#' Takes a dataframe containing addresses as an input and returns 
#' the results from a specified geocoding service in a dataframe format using the
#' [geo] function. See example usage in `vignette("tidygeocoder")`.
#' 
#' This function passes all additional parameters (`...`) to the 
#' [geo] function, so you can refer to its documentation for more details
#' on possible arguments.
#' 
#' Note that the arguments used for specifying address columns (`address`, 
#' `street`, `city`, `county`, `state`, `postalcode`, and `country`) accept either
#' quoted or unquoted column names (ie. `"address_col"` and `address_col` are 
#' both acceptable). 
#'
#' @param .tbl dataframe containing addresses
#' @param address single line street address column name. Do not combine with 
#'  address component arguments (`street`, `city`, `county`, `state`, `postalcode`, `country`)
#' @param street street address column name
#' @param city city column name
#' @param county county column name
#' @param state state column name
#' @param postalcode postalcode column name (zip code if in the United States)
#' @param country country column name
#' 
#' @param lat latitude column name. Can be quoted or unquoted (ie. lat or 'lat').
#' @param long longitude column name. Can be quoted or unquoted (ie. long or 'long').
#' @param return_input if TRUE then the input dataset will be combined with the geocoder query results 
#'   and returned. If FALSE only the geocoder results will be returned.
#' @param return_addresses if TRUE return input addresses. Defaults to TRUE if `return_input` is FALSE
#'   and FALSE if `return_input` is TRUE. This argument is passed to the `geo()` function.
#' @param unique_only if TRUE then only unique results will be returned and 
#'   return_input will be set to FALSE.
#' @param limit `r get_limit_documentation(reverse = FALSE, df_input = TRUE)`
#' @param ... arguments passed to the [geo] function
#' @inherit geo return
#'
#' @examples
#' \donttest{
#' library(dplyr, warn.conflicts = FALSE)
#' sample_addresses %>% slice(1:2) %>%
#'  geocode(addr, method = 'arcgis')
#'
#' louisville %>% head(2) %>%
#'  geocode(street = street, city = city, state = state,
#'   postalcode = zip, method = 'census', full_results = TRUE)
#'
#' sample_addresses %>% slice(8:9) %>%
#'  geocode(addr, method = 'osm', limit = 2,
#'   return_input = FALSE, full_results = TRUE)
#'
#' sample_addresses %>% slice(4:5) %>%
#'  geocode(addr, method = 'arcgis',
#'   lat = latitude, long = longitude,
#'   full_results = TRUE)
#' }
#' @seealso [geo]
#' @export
geocode <- function(.tbl, address = NULL, street = NULL, city = NULL, county = NULL, 
                    state = NULL, postalcode = NULL, country = NULL,
                    lat = lat, long = long, return_input = TRUE, limit = 1, return_addresses = NULL, unique_only = FALSE, ...) {
  
  # Non-standard evaluation --------------------------------------------------------------
  # Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  if(!is.null(substitute(address)))     address    <- rm_quote(deparse(substitute(address)))
  if(!is.null(substitute(street)))      street     <- rm_quote(deparse(substitute(street)))
  if(!is.null(substitute(city)))        city       <- rm_quote(deparse(substitute(city)))
  if(!is.null(substitute(county)))      county     <- rm_quote(deparse(substitute(county)))
  if(!is.null(substitute(state)))       state      <- rm_quote(deparse(substitute(state)))
  if(!is.null(substitute(postalcode)))  postalcode <- rm_quote(deparse(substitute(postalcode)))
  if(!is.null(substitute(country)))     country    <- rm_quote(deparse(substitute(country)))
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  stopifnot(is.logical(return_input), is.logical(unique_only), is.null(return_addresses) || is.logical(return_addresses))
  
  if (unique_only == TRUE) return_input <- FALSE
  
  # if return_addresses is NULL (default) then set based on return_input
  if (is.null(return_addresses)) return_addresses <- if (return_input == TRUE) FALSE else TRUE
  
  # capture all function arguments including default values as a named list
  all_args <- as.list(environment())

  if (!(is.data.frame(.tbl))) {
    stop('.tbl is not a dataframe. See ?geocode', call. = FALSE)
  }
  
  # This check prevents a address-results misalignment issue https://github.com/jessecambon/tidygeocoder/issues/88
  check_limit_return_input(limit, return_input)
  
  # convert .tbl to tibble if it isn't one already
  .tbl <- tibble::as_tibble(.tbl)
  
  # put all non-NULL address components into a named list
  # create address parameters to be passed to the geo function as a named list of lists
  addr_parameters <- list()
  for (var in pkg.globals$address_arg_names) {
    if (!is.null(all_args[[var]])) {
      
      # throw error if the an address parameter doesn't specify a column in the dataset
      if (!(all_args[[var]] %in% colnames(.tbl))) {
        stop(paste0('"', all_args[[var]], '" is not a column name in the input dataset.'), call. = FALSE)
      }
      addr_parameters[[var]] <- .tbl[[all_args[[var]]]]
    }
  }
  
  # Arguments to pass to geo()
  # remove any arguments specific to geocode() that geo() doesn't have
  geo_args <- c(addr_parameters, 
      all_args[!names(all_args) %in% c('.tbl', 'return_input', pkg.globals$address_arg_names)], list(...))
  
  # Pass addresses to the geo function
  results <- do.call(geo, geo_args)
  
  if (return_input == FALSE) {
    return(results)
  } else {
    # cbind the original dataframe to the coordinates and convert to tibble
    # change column names to be unique if there are duplicate column names
    return(dplyr::bind_cols(.tbl, results))
  }
}
