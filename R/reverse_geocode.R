#' Reverse geocode coordinates in a dataframe
#'
#' @description
#' Takes a dataframe containing coordinates (latitude and longitude) and returns 
#' the reverse geocoding query results from a specified service by using the
#' \code{\link{reverse_geo}} function. See example usage in \code{vignette("tidygeocoder")}.
#' 
#' This function passes all additional parameters (\code{...}) to the 
#' \code{\link{reverse_geo}} function, so you can refer to its documentation for more details
#' on possible arguments.
#'
#' @param .tbl dataframe containing coordinates
#' @param lat latitude column name (input data). Can be quoted or unquoted (ie. lat or 'lat').
#' @param long longitude column name (input data). Can be quoted or unquoted (ie. long or 'long').
#' @param address address column name (output data). Can be quoted or unquoted (ie. addr or 'addr').
#' @param limit --------------------------
#' @param return_coords if TRUE then coordinates with standard names will be returned
#'   This is defaulted to FALSE because the coordinates are already in the input dataset
#' @param unique_only if TRUE then only unique coordinates and results will be returned. 
#'   The input dataframe's format is not preserved. Coordinates will also be returned if 
#'   TRUE (overrides return_coords argument).
#' @param ... arguments passed to the \code{\link{reverse_geo}} function
#' @return input dataframe (.tbl) with geocoder results appended as columns
#'
#' @examples
#' \donttest{
#' library(tibble)
#' reverse_geocode(
#'   tibble(
#'     latitude = c(38.895865, 43.6534817),
#'     longitude = c(-77.0307713,-79.3839347)
#'   ),
#'   lat = latitude,
#'   long = longitude,
#'   method = 'osm',
#'   full_results = TRUE,
#'   verbose = TRUE
#' )
#' }
#' @seealso \code{\link{reverse_geo}} \code{\link{api_parameter_reference}}
#' @export
reverse_geocode <- function(.tbl, lat, long, address = address, limit = 1, return_coords = FALSE, unique_only = FALSE, ...) {
  
  # Non-standard evaluation --------------------------------------------------------------
  # Quote unquoted vars without double quoting quoted vars
  # end result - all of these variables become character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  address <- rm_quote(deparse(substitute(address)))
  
  if (unique_only == TRUE) return_coords <- TRUE
  
  # capture all function arguments including default values as a named list
  all_args <- as.list(environment())
  
  if (!(is.data.frame(.tbl))) {
    stop('.tbl is not a dataframe. See ?reverse_geocode')
  }
  
  if (limit != 1 & return_coords == FALSE & unique_only == FALSE) {
    stop('To use limit > 1 then either set return_coords or unique_only to TRUE.')
  }
  
  # convert .tbl to tibble if it isn't one already
  .tbl <- tibble::as_tibble(.tbl)
  
  coord_parameters <- list()
  # put all non-lat,long arguments into a named list
  # create address parameters to be passed to the geo function as a named list of lists
  for (var in c('lat', 'long')) {
      # throw error if the an address parameter doesn't specify a column in the dataset
      if (!(all_args[[var]] %in% colnames(.tbl))) {
        stop(paste0('"', all_args[[var]], '" is not a column name in the input dataset.'))
      }
      coord_parameters[[var]] <- .tbl[[all_args[[var]]]]
  }
  
  # Arguments to pass to reverse_geo()
  reverse_geo_args <- c(coord_parameters, 
                all_args[!names(all_args) %in% c('.tbl', 'lat', 'long')], list(...))
  
  # Pass addresses to the reverse_geo function
  results <- do.call(reverse_geo, reverse_geo_args)
  
  if (unique_only == TRUE | return_coords == TRUE) {
    return(results)
  } else {
    # cbind the original dataframe to the coordinates and convert to tibble
    # change column names to be unique if there are duplicate column names
    return(dplyr::bind_cols(.tbl, results))
  }
}

# d <- reverse_geocode(tibble(latitude = c(38.895865, 43.6534817), longitude = c(-77.0307713, -79.3839347)), 
# lat = latitude, long = longitude, method = 'osm', full_results = TRUE, verbose = TRUE)