# .tbl = dataframe
# lat, long = names of lat an long columns in .tbl 
#' @export
reverse_geocode <- function(.tbl, lat, long, address = address, return_coords = FALSE, unique_only = FALSE, ...) {
  
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
  
  # Arguments to pass to geo()
  reverse_geo_args <- c(coord_parameters, 
                all_args[!names(all_args) %in% c('.tbl', 'lat', 'long')], list(...))
  
  # Pass addresses to the geo function
  results <- do.call(reverse_geo, reverse_geo_args)
  
  if (unique_only == TRUE) {
    return(results)
  } else {
    # cbind the original dataframe to the coordinates and convert to tibble
    # change column names to be unique if there are duplicate column names
    return(tibble::as_tibble(cbind(.tbl, results)))
  }
}

# d <- reverse_geocode(tibble(latitude = c(38.895865, 43.6534817), longitude = c(-77.0307713, -79.3839347)), 
# lat = latitude, long = longtitude, method = 'osm', full_results = TRUE, verbose = TRUE)