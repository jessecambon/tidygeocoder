#' Geocode addresses in a dataframe
#'
#' Takes a dataframe containing addresses as a input. Returns
#' the dataframe with latitude and longitude coordinate columns
#' using a user specified geocoder function.
#'
#' See example usage in \code{vignette("tidygeocoder")}
#'
#' @param .tbl dataframe
#' 
#' Names of columns in the dataframe:
#' @param address single line street address
#' @param street street address
#' @param city city
#' @param county county
#' @param state state
#' @param postalcode postalcode (zip code if in the United States)
#' @param country country
#' @param method the geocoder function you want to use
#' \itemize{
#'   \item "census": only for US street level addresses
#'   \item "osm": worldwide coverage but has a usage limit
#' }
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param verbose toggle verbose output
#' @param ... arguments passed to the \code{\link{geo}} function
#' @return input dataframe (.tbl) with latitude and longitude fields appended
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' sample_addresses %>% geocode(addr)
#'
#' sample_addresses %>% geocode(addr,method='cascade',lat=latitude,long=longitude)
#' }
#' @importFrom tibble tibble as_tibble
#' @export
geocode <- function(.tbl, address = NULL, street=NULL, city = NULL, county = NULL, 
                    state = NULL, postalcode = NULL, country = NULL,
                    method='census', lat = lat, long = long, verbose = FALSE, ...) {
  
  # NSE - Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  address <- rm_quote(deparse(substitute(address)))
  street <- rm_quote(deparse(substitute(street)))
  city <- rm_quote(deparse(substitute(city)))
  county <- rm_quote(deparse(substitute(county)))
  state <- rm_quote(deparse(substitute(state)))
  postalcode <- rm_quote(deparse(substitute(postalcode)))
  country <- rm_quote(deparse(substitute(country)))
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # capture all function arguments including default values as a named list
  all_args <- as.list(environment())
  
  start_time <- Sys.time() # start timer
  address_arg_names <- c('address', 'street', 'city', 'county', 'state', 'postalcode', 'country')
  
  # put all non-NULL address components into a named list
  # create address parameters to be passed to the geo function as a named list of lists
  addr_parameters <- list()
  for (var in address_arg_names) {
    if (var != "NULL") addr_parameters[[var]] <- .tbl[[all_args[[var]]]]
  }
  geo_args <- c(addr_parameters, all_args[!names(all_args) %in% c('.tbl',address_arg_names)], list(...))
  
  # Pass addresses to the geo function
  coordinates <- do.call(geo, geo_args)
  
  # cbind the original dataframe to the coordinates and convert to tibble
  # change column names to be unique if there are duplicate column names
  final_df <- tibble::as_tibble(cbind(.tbl,coordinates))

  if (verbose == TRUE) print_time("Query executed in", get_seconds_elapsed(start_time))
  
  return(final_df)
}
