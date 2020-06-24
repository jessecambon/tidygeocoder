#' Geocode addresses in a dataframe
#'
#' Takes a dataframe containing addresses as a input. It returns
#' the dataframe with latitude and longitude coordinate columns
#' using a user specified geocoder service. This function passes
#' all additional parameters (...) to the \code{\link{geo}} function
#' so you can refer to its documentation for more details on possible
#' arguments.
#'
#' See example usage in \code{vignette("tidygeocoder")}
#'
#' @param .tbl dataframe containing addresses
#' 
#' Names of columns in the dataframe:
#' @param address single line street address column name. Do not combine with 
#'  address component arguments (street, city, county, state, postalcode, country)
#' @param street street address column name
#' @param city city column name
#' @param county county column name
#' @param state state column name
#' @param postalcode postalcode column name (zip code if in the United States)
#' @param country country column name
#' 
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param return_addresses if TRUE then addresses with standard names will be returned
#'   This is defaulted to FALSE because the address fields are already in the input dataset
#' @param ... arguments passed to the \code{\link{geo}} function
#' @return input dataframe (.tbl) with geocoder results appended as columns
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' sample_addresses[1:3,] %>% geocode(addr)
#' 
#' louisville[1:2, ] %>% geocode(street = street, city = city, state = state,
#'   postalcode = zip)
#' 
#' sample_addresses[8:9,] %>% geocode(addr, method='osm',lat='lattes', long='longos')
#'
#' sample_addresses[1:3,] %>% geocode(addr, method='cascade', lat=latitude, long=longitude)
#' }
#' @export
geocode <- function(.tbl, address = NULL, street = NULL, city = NULL, county = NULL, 
                    state = NULL, postalcode = NULL, country = NULL,
                    lat = lat, long = long, return_addresses = FALSE, ...) {
  
  # NSE - Quote unquoted vars without double quoting quoted vars
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
  
  # capture all function arguments including default values as a named list
  all_args <- as.list(environment())
  start_time <- Sys.time() # start timer
  
  # put all non-NULL address components into a named list
  # create address parameters to be passed to the geo function as a named list of lists
  addr_parameters <- list()
  for (var in pkg.globals$address_arg_names) {
    if (!is.null(all_args[[var]])) addr_parameters[[var]] <- .tbl[[all_args[[var]]]]
  }
  geo_args <- c(addr_parameters, 
      all_args[!names(all_args) %in% c('.tbl', pkg.globals$address_arg_names)], list(...))
  
  # Pass addresses to the geo function
  results <- do.call(geo, geo_args)
  
  # cbind the original dataframe to the coordinates and convert to tibble
  # change column names to be unique if there are duplicate column names
  final_df <- tibble::as_tibble(cbind(.tbl, results))

  #if (verbose == TRUE) print_time("Query executed in", get_seconds_elapsed(start_time))
  
  return(final_df)
}
