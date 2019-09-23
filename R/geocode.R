#' Geocode a street address
#'
#' Returns latitude and longitude as two columns appended to
#' the inputted dataframe
#'
#' @param .tbl Dataframe
#' @param address Single line address. Street must be included.
#' @param method What geocoder api do you want to use? 'OSM' or 'Census'
#' @param ... arguments supplied to the relevant geocoder function
#' (see \code{\link{geo_census}} (Census) and \code{\link{geo_osm}} (OSM))
#' @return Latitude and Longitude Coordinates in tibble format
#'
#' @examples
#' \dontrun{
#' sample_addresses %>% geocode(addr)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate case_when bind_cols pull
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom rlang enquo
#' @export
geocode <- function(.tbl,address,method='Census',...) {
  address=rlang::enquo(address)

  temp = NULL

  # func <- dplyr::case_when(method == 'OSM' ~ rlang::enquo(geo_osm),
  #                          TRUE ~ rlang::enquo(geo_census))

  coords <- tibble(temp=purrr::map(.tbl %>% dplyr::pull(!!address),geo_census,...)) %>%
    tidyr::unnest(temp,keep_empty=TRUE)

  .tbl %>% dplyr::bind_cols(coords)
}
