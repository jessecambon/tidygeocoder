#' Geocode street addresses
#'
#' Obtain latitude and longitude coordinates from an address using the US Census geocoder.
#' Only works for addresses within the US. Addresses must also be at the street
#' level (ie. 60 Main St. Pawnee, IN not Pawnee, IN).
#' 
#' @references \href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census API Documentation}
#'
#' @param address single line address. Street must be included.
#' @param lat name of latitude field
#' @param long name of longitude field
#' @param verbose logical. If TRUE outputs logs.
#' @param api_url URL of Census API
#' @param benchmark parameter for the US Census Geocoder
#' @return latitude and longitude coordinates in tibble format
#'
#' @examples
#' \donttest{
#' geo_census("1600 Pennsylvania Ave Washington, DC")
#' }
#' @importFrom tibble tibble
#' @importFrom stringr str_trim
#' @export
geo_census <- function(address, lat = lat, long = long, verbose=FALSE,
       benchmark=4,
       api_url="https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"){
  lat <- deparse(substitute(lat))
  long <- deparse(substitute(long))

  # what to return when we don't find results
  NA_value <- get_na_value(lat,long)

  if (verbose == TRUE) { message(address) }

  # if address is NA, numeric, or blank then return NA, else make call to census geocoder
  if (!is.na(suppressWarnings(as.numeric(address))) | !is.character(address) | is.na(address) | stringr::str_trim(address) == "") {
    if (verbose == TRUE) { message("Blank or missing address!") }
    return(NA_value)
  }
    # API Call
    dat <- query_api(api_url, 
              list(address = address, format = 'json', benchmark = benchmark), 
              content_encoding = "ISO-8859-1")

    # extract coordinates
    coords_xy <- dat$result$addressMatches$coordinates[1,]

    # Return coordinates in tibble form
    if (!is.null(coords_xy)) {
      lat_lng <- tibble::tibble(a = coords_xy$y, b = coords_xy$x)
      colnames(lat_lng) <- c(lat, long)
      return(lat_lng) 
    }
    else return(NA_value) 
}
