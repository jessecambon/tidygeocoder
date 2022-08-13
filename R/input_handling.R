# By default tidygeocoder only sends unique inputs to geocoding services
# but preserves duplicates in outputs. These functions perform the
# deduplication and "re-deduplication" to facilitate that.


# utility function for packaging either lat longs or address data
# takes a dataframe input
# cords = TRUE if processing coordinates
# note that reverse_geo() converts lat and long into numeric before passing to this function
package_inputs <- function(input_orig, coords = FALSE) {
  input_colnames <- names(input_orig) # store column names

  input_unique <- input_orig

  # limit lat/longs in unique dataset to possible values
  if (coords == TRUE) {
    input_unique$lat[(input_unique$lat > 90 | input_unique$lat < -90)] <- as.numeric(NA)
    input_unique$long[(input_unique$long > 180 | input_unique$long < -180)] <- as.numeric(NA)

    # If lat is NA then make long NA and vice versa (ie. don't pass coordinate if
    # only one value exists)
    input_unique$lat <- ifelse(is.na(input_unique$long), as.numeric(NA), input_unique$lat)
    input_unique$long <- ifelse(is.na(input_unique$lat), as.numeric(NA), input_unique$long)
  }

  # remove rows that are entirely blank or NA
  input_unique <- input_unique[!apply(is.na(input_unique) | input_unique == "", 1, all), ]

  # only keep unique rows and then create a unique identifier column
  input_unique <- unique(input_unique)

  # if there are 0 valid/nonblank inputs... return a tibble with 1 row for unique
  # and a crosswalk the same length as the input dataframe
  if (nrow(input_unique) == 0) {
    NA_list <- as.list(rep(NA, length(input_colnames)))
    names(NA_list) <- input_colnames
    return(list(
      unique = tibble::as_tibble(NA_list),
      crosswalk = tibble::tibble(.id = 1:nrow(input_orig), .uid = 1)
    ))
  }

  # Create unique identifiers
  input_unique[[".uid"]] <- 1:nrow(input_unique)
  # create id to record original input dataset order
  input_orig[[".id"]] <- 1:nrow(input_orig)

  # crosswalk
  crosswalk <- merge(input_orig, input_unique, by = input_colnames, all.x = TRUE, sort = FALSE)
  crosswalk <- crosswalk[order(crosswalk[[".id"]]), ] # reorder

  # Return a named list containing two dataframes.
  # unique contains all the unique inputs to pass to the geocoder service
  # crosswalk contains only the .uid and .id columns to match the geocoder results
  # back to the original input data (which may contain duplicates/blanks/etc)
  return(list(
    unique = tibble::as_tibble(input_unique[!names(input_unique) %in% c(".uid")]),
    crosswalk = tibble::as_tibble(crosswalk[!names(crosswalk) %in% input_colnames])
  ))
}

# Function for packaging and deduping addresses that are passed to the geo function
# package addresses
package_addresses <- function(address = NULL,
                              street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL) {

  # package all non-NULL address arguments into a named list
  combined_addr <- as.list(environment())
  combined_addr[sapply(combined_addr, is.null)] <- NULL # remove NULL items
  # apply trimws to non-NA values
  # (trimws converts to character so it turns NA into the 'NA' character value)
  combined_addr <- lapply(combined_addr, function(x) ifelse(is.na(x), NA, trimws(x)))

  arg_names <- names(combined_addr)

  ## QA check the address inputs
  if (("address" %in% arg_names) & (length(arg_names) > 1)) {
    stop("Do not use other address component parameters with the single line 'address' parameter", call. = FALSE)
  }
  address_component_lengths <- lengths(combined_addr)
  if (max(address_component_lengths) != min(address_component_lengths)) {
    stop("Address components must be equal in length", call. = FALSE)
  }

  # Turn address inputs into a dataframe
  addr_orig <- tibble::as_tibble(combined_addr)

  # package and return
  return(package_inputs(tibble::as_tibble(combined_addr)))
}

# Function for unpackaging and RE-deduping inputs (either addresses or lat/long coordinates)
# so that we can return them in the same order that they were passed
# this function assumes that the results are in the same order as package$unique
# Args:
#   package: the output of package_inputs() or package_addresses()
#   results: the results returned by the geocoding service
#   unique_only: if TRUE then only unique results are returned
#   return_inputs: if TRUE then include inputs in the returned results
#
# @export
unpackage_inputs <- function(package, results, unique_only = FALSE, return_inputs = FALSE) {

  # Add addresses to results if we are returning them
  if (return_inputs == TRUE) results <- dplyr::bind_cols(package$unique, results)

  # if there are no duplicates then just return the raw results
  if ((nrow(package$unique) == nrow(package$crosswalk)) || unique_only == TRUE) {
    return(tibble::as_tibble(results))
  }

  # If there are duplicates then we need to use the crosswalk to return results properly
  id_colnames <- names(package$crosswalk)

  # on the off chance that the results dataset has the
  # id colnames in it then we remove them
  results <- results[!names(results) %in% id_colnames]

  # create unique id column in results
  results[[".uid"]] <- 1:nrow(results)

  # join crosswalk and results
  # had to use dplyr::left_join instead of merge() due to errors when the datasets
  # contain NA values and nested dataframes
  base <- dplyr::left_join(package$crosswalk, results, by = ".uid")
  base <- base[order(base[[".id"]]), ] # reorder just in case

  return(tibble::as_tibble(base[!names(base) %in% id_colnames]))
}
