#' Reverse geocode coordinates in a dataframe
#'
#' @description
#' Takes a dataframe containing coordinates (latitude and longitude) and returns 
#' the reverse geocoding query results from a specified service by using the
#' [reverse_geo] function. See example usage in `vignette("tidygeocoder")`.
#' 
#' This function passes all additional parameters (`...`) to the 
#' [reverse_geo] function, so you can refer to its documentation for more details
#' on possible arguments.
#'
#' @param .tbl dataframe containing coordinates
#' @param lat latitude column name (input data). Can be quoted or unquoted (ie. lat or 'lat').
#' @param long longitude column name (input data). Can be quoted or unquoted (ie. long or 'long').
#' @param address address column name (output data). Can be quoted or unquoted (ie. addr or 'addr').
#' @inheritParams geocode
#' @param limit `r get_limit_documentation(reverse = TRUE, df_input = TRUE)`
#' @param return_coords if TRUE return input coordinates. Defaults to TRUE if `return_input` is 
#'    FALSE and FALSE if `return_input` is TRUE. This argument is passed to the `reverse_geo()` function.
#' @param ... arguments passed to the [reverse_geo] function
#' @inherit geo return
#'
#' @examples
#' \donttest{
#' library(tibble)
#' library(dplyr)
#' 
#' tibble(
#'     latitude = c(38.895865, 43.6534817),
#'     longitude = c(-77.0307713,-79.3839347)
#'   ) %>%
#'   reverse_geocode(
#'     lat = latitude,
#'     long = longitude,
#'     method = 'osm',
#'     full_results = TRUE
#'   )
#' 
#' louisville %>% head(3) %>% 
#'   reverse_geocode(lat = latitude, long = longitude, 
#'   method = 'arcgis')
#' 
#' louisville %>% head(2) %>% 
#'   reverse_geocode(lat = latitude, long = longitude,  
#'   method = 'osm', verbose = TRUE,
#'   limit = 2, return_input = FALSE)
#' 
#' }
#' @seealso [reverse_geo]
#' @export
reverse_geocode <- function(.tbl, lat, long, address = address, return_input = TRUE, limit = 1, return_coords = NULL, unique_only = FALSE, ...) {
  
  # Non-standard evaluation --------------------------------------------------------------
  # Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  address <- rm_quote(deparse(substitute(address)))
  
  stopifnot(is.logical(return_input), is.logical(unique_only), is.null(return_coords) || is.logical(return_coords))
  
  if (unique_only == TRUE) return_input <- FALSE
  
  # if return_addresses is NULL (default) then set based on return_input
  if (is.null(return_coords)) return_coords <- if (return_input == TRUE) FALSE else TRUE
  
  # capture all function arguments including default values as a named list
  all_args <- as.list(environment())
  
  if (!(is.data.frame(.tbl))) {
    stop('.tbl is not a dataframe. See ?reverse_geocode', call. = FALSE)
  }
  
  # This check prevents a address-results misalignment issue https://github.com/jessecambon/tidygeocoder/issues/88
  check_limit_return_input(limit, return_input)
  
  # convert .tbl to tibble if it isn't one already
  .tbl <- tibble::as_tibble(.tbl)
  
  coord_parameters <- list()
  # put all non-lat,long arguments into a named list
  # create address parameters to be passed to the geo function as a named list of lists
  for (var in c('lat', 'long')) {
      # throw error if the an address parameter doesn't specify a column in the dataset
      if (!(all_args[[var]] %in% colnames(.tbl))) {
        stop(paste0('"', all_args[[var]], '" is not a column name in the input dataset.'), call. = FALSE)
      }
      coord_parameters[[var]] <- .tbl[[all_args[[var]]]]
  }
  
  # Arguments to pass to reverse_geo()
  # remove any arguments specific to geocode() that reverse_geo() doesn't have
  reverse_geo_args <- c(coord_parameters, 
                all_args[!names(all_args) %in% c('.tbl', 'lat', 'long', 'return_input')], list(...))
  
  # Pass addresses to the reverse_geo function
  results <- do.call(reverse_geo, reverse_geo_args)
  
  if (return_input == FALSE) {
    return(results)
  } else {
    # cbind the original dataframe to the coordinates and convert to tibble
    return(dplyr::bind_cols(.tbl, results))
  }
}