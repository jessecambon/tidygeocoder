#' Geocode street addresses in a tibble
#'
#' Returns latitude and longitude as two columns appended to
#' the inputted dataframe.
#' See example usage in vignette("geocode")
#'
#' @param .tbl Dataframe
#' @param address Single line address.
#' \itemize{
#'   \item "census": \code{\link{geo_census}} - can only handle US street level addresses
#'   \item "osm": \code{\link{geo_osm}} - more versatile than Census but has a usage limit
#'   \item "cascade": \code{\link{geo_cascade}} - first tries to use census then tries osm
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
#' @importFrom dplyr '%>%' mutate case_when bind_cols pull rename
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom rlang enquo
#' @export
geocode <- function(.tbl,address,method='census',lat=lat,long=long,...) {
  latitude  <- longitude <- temp <- NULL # prevents 'no visible binding for global variable' warning

  address<- rlang::enquo(address)
  lat <- rlang::enquo(lat)
  long <- rlang::enquo(long)

  # Select geolocation method to use
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

  # use the selected geolocation method to return coordinates and attach them
  # to the input dataframe
  coords <- tibble(temp=purrr::map(.tbl %>% dplyr::pull(!!address),
                      !!func,
                      lat=!!lat,long=!!long,...)) %>%
    tidyr::unnest(temp,keep_empty=TRUE)

  .tbl %>% dplyr::bind_cols(coords)
}
