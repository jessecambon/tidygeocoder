#' Geocode street addresses in a dataframe
#'
#' Takes a dataframe containing addresses as a input. Returns
#' the dataframe with latitude and longitude coordinate columns
#' using a user specified geocoder function.
#'
#' See example usage in \code{vignette("tidygeocoder")}
#'
#' @param .tbl dataframe
#' @param address name of column containing addresses in .tbl
#' @param method the geocoder function you want to use
#' \itemize{
#'   \item "census": only for US street level addresses
#'   \item "osm": worldwide coverage but has a usage limit
#' }
#' @param lat name of latitude field
#' @param long name of longitude field
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
#' @importFrom tibble tibble
#' @export
geocode <- function(.tbl, address , method='census', lat = lat, long = long, ...) {

  address <- deparse(substitute(address)) 
  lat <- deparse(substitute(lat))
  long <- deparse(substitute(long))
  
  # Extract list of addresses
  address_list <- as.list(.tbl[[address]])
  
  # Apply our selected geocoder function to the list of addresses
  # This returns a list of tibble dataframes of 2 columns and 1 row each
  list_coords <- lapply(address_list,geo, method = method, ...)
  
  # rbind the list of tibble dataframes together
  coordinates <- do.call('rbind',list_coords)
  
  # Set column names of our coordinates
  colnames(coordinates) <- c(lat, long)
  
  # cbind the original dataframe to the coordinates
  final_df <- cbind(.tbl,coordinates)

  return(final_df)
}
