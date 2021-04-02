#' Geocode addresses in a dataframe
#'
#' @description
#' Takes a dataframe containing addresses as an input and returns 
#' the results from a specified geocoder service in a dataframe format using the
#' [geo] function. See example usage in `vignette("tidygeocoder")`.
#' 
#' This function passes all additional parameters (`...`) to the 
#' \code{\link{geo}} function, so you can refer to its documentation for more details
#' on possible arguments.
#' 
#' Note that the arguments used for specifying address columns (address, 
#' street, city, county, state, postalcode, and country) accept either
#' quoted or unquoted column names (ie. "address_col" and address_col are 
#' both acceptable). 
#'
#' @param .tbl dataframe containing addresses
#' @param address single line street address column name. Do not combine with 
#'  address component arguments (street, city, county, state, postalcode, country)
#' @param street street address column name
#' @param city city column name
#' @param county county column name
#' @param state state column name
#' @param postalcode postalcode column name (zip code if in the United States)
#' @param country country column name
#' 
#' @param lat latitude column name. Can be quoted or unquoted (ie. lat or 'lat').
#' @param long longitude column name. Can be quoted or unquoted (ie. long or 'long').
#' @param limit `r get_limit_documentation(reverse = FALSE, df_input = TRUE)`
#' @inheritParams geo
#' @param ... arguments passed to the [geo] function
#' @inherit geo return
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' sample_addresses[1:2,] %>% geocode(addr)
#' 
#' louisville[1:2,] %>% geocode(street = street, city = city, state = state,
#'   postalcode = zip)
#' 
#' sample_addresses[8:9,] %>% geocode(addr, method = 'osm',
#'   lat = 'lattes', long = 'longos')
#'
#' sample_addresses[4:5,] %>% geocode(addr, method = 'cascade',
#'   lat = latitude, long = longitude)
#' }
#' @seealso \code{\link{geo}}
#' @export
geocode <- function(.tbl, address = NULL, street = NULL, city = NULL, county = NULL, 
                    state = NULL, postalcode = NULL, country = NULL,
                    lat = lat, long = long, limit = 1, return_addresses = FALSE, unique_only = FALSE, ...) {
  
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
  
  if (unique_only == TRUE) return_addresses <- TRUE
  
  # capture all function arguments including default values as a named list
  all_args <- as.list(environment())

  if (!(is.data.frame(.tbl))) {
    stop('.tbl is not a dataframe. See ?geocode')
  }
  
  # This check prevents a address-results misalignment issue https://github.com/jessecambon/tidygeocoder/issues/88
  if ((is.null(limit) || limit != 1) && return_addresses == FALSE && unique_only == FALSE) {
    stop('To use limit > 1 or limit = NULL, set either return_addresses or unique_only to TRUE.')
  }
  
  # convert .tbl to tibble if it isn't one already
  .tbl <- tibble::as_tibble(.tbl)
  
  # put all non-NULL address components into a named list
  # create address parameters to be passed to the geo function as a named list of lists
  addr_parameters <- list()
  for (var in pkg.globals$address_arg_names) {
    if (!is.null(all_args[[var]])) {
      
      # throw error if the an address parameter doesn't specify a column in the dataset
      if (!(all_args[[var]] %in% colnames(.tbl))) {
        stop(paste0('"', all_args[[var]], '" is not a column name in the input dataset.'))
      }
      addr_parameters[[var]] <- .tbl[[all_args[[var]]]]
    }
  }
  
  # Arguments to pass to geo()
  geo_args <- c(addr_parameters, 
      all_args[!names(all_args) %in% c('.tbl', pkg.globals$address_arg_names)], list(...))
  
  # Pass addresses to the geo function
  results <- do.call(geo, geo_args)
  
  if (unique_only == TRUE | return_addresses == TRUE) {
    return(results)
  } else {
    # cbind the original dataframe to the coordinates and convert to tibble
    # change column names to be unique if there are duplicate column names
    return(dplyr::bind_cols(.tbl, results))
  }
}
