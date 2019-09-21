#' Geocode a street address
#'
#' Returns latitude and longitude as two columns appended to
#' the inputted dataframe
#'
#' @param .tbl Dataframe
#' @param address Single line address. Street must be included.
#' @param method What geocoder api do you want to use? 'OSM' or 'Census'
#' @param verbose logical. If TRUE outputs logs.
#' @return Latitude and Longitude Coordinates in tibble format
#'
#' @examples
#'
#' @importFrom tibble tibble
#' @importFrom dplyr '%>%' mutate case_when bind_cols pull
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom rlang enquo
#' @export
geocode <- function(.tbl,address,method='Census',verbose=FALSE) {
  address=rlang::enquo(address)

  temp = NULL

  # func <- dplyr::case_when(method == 'OSM' ~ rlang::enquo(geo_osm),
  #                          TRUE ~ rlang::enquo(geo_census))

  coords <- tibble(temp=purrr::map(.tbl %>% dplyr::pull(!!address),geo_census,verbose=TRUE)) %>%
    tidyr::unnest(temp,keep_empty=TRUE)

  .tbl %>% dplyr::bind_cols(coords)
}
# USAGE : tibble::tibble(addr='1600 Pennsylvania Ave Washington, DC') %>% geocode(addr)
