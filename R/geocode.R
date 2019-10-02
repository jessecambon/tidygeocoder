#' Tidyverse-style interface for geocoding addresses
#'
#' Returns latitude and longitude as two columns appended to
#' the inputted dataframe.
#' See example usage in vignette("geocode")
#'
#' @param .tbl Dataframe
#' @param address Single line address. Street must be included.
#' @param method the geocoder function/service that you want to use
#' \itemize{
#'   \item "census": \code{\link{geo_census}}
#'   \item "osm": \code{\link{geo_osm}}
#'   \item "cascade": \code{\link{geo_cascade}} (first tries to use census then tries osm)
#' }
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param ... arguments supplied to the relevant geocoder function
#' @return input dataframe (.tbl) with latitude and longitude fields appended
#'
#' @examples
#' \dontrun{
#' sample_addresses %>% geocode(addr)
#'
#' sample_addresses %>% geocode(addr,method='cascade',lat=latitude,long=longitude)
#' }
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate case_when bind_cols pull
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom rlang enquo
#' @export
geocode <- function(.tbl,address,method='census',lat=lat,long=lng,...) {
  address<- rlang::enquo(address)
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  temp = NULL

  if (method == 'census') {
    func <- rlang::enquo(geo_census)
  } else if (method == 'osm') {
    func <- rlang::enquo(geo_osm)
  } else if (method == 'cascade') {
    func <- rlang::enquo(geo_cascade)
  } else {
    print('Warning: unknown method, defaulting to census')
    func <- rlang::enquo(geo_census)
  }

  coords <- tibble(temp=purrr::map(.tbl %>% dplyr::pull(!!address),
                      !!func,
                      lat=!!lat,long=!!long,...)) %>%
    tidyr::unnest(temp,keep_empty=TRUE)

  .tbl %>% dplyr::bind_cols(coords)
}
