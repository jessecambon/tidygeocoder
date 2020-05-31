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
#' @importFrom tibble tibble as_tibble
#' @export
geocode <- function(.tbl, address , method='census', mode='auto', lat = lat, long = long, verbose = FALSE, ...) {
  start_time <- Sys.time() # start timer
  
  # NSE - Quoted unquoted vars without double quoting quoted vars
  address <- gsub("\"","", deparse(substitute(address)))
  lat <- gsub("\"","", deparse(substitute(lat)))
  long <- gsub("\"","", deparse(substitute(long)))
  
  # Extract list of addresses
  address_list <- as.list(.tbl[[address]])
  
  # Pass addresses to the geo function
  coordinates <- geo(unlist(address_list), method = method, lat = lat, long = long, ...)
  
  # cbind the original dataframe to the coordinates and convert to tibble
  # change column names to be unique if there are duplicate column names
  final_df <- tibble::as_tibble(cbind(.tbl,coordinates), .name_repair = 'unique')

  if (verbose == TRUE) print_time("Query executed in", get_seconds_elapsed(start_time))
  
  return(final_df)
}
