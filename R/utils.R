### Put common utility functions here

## Declare global variables
pkg.globals <- new.env()
pkg.globals$address_arg_names <- c('address', 'street', 'city', 'county', 'state', 'postalcode', 'country')

#' Extract forward geocoding results 
#' 
#' @description
#' Parses the output of the \code{\link{query_api}} function.
#' Latitude and longitude are extracted into the first two columns
#' of the returned dataframe. This function is not used for batch 
#' geocoded results. Refer to \code{\link{query_api}} for example
#' usage.
#' 
#' @param method method name
#' @param response  content from the geocoder service (returned by the \code{\link{query_api}} function)
#' @param full_results if TRUE then the full results (not just latitude and longitude)
#'   will be returned.
#' @param flatten if TRUE then flatten any nested dataframe content
#' @return geocoder results in tibble format 
#' @seealso \code{\link{get_api_query}} \code{\link{query_api}} \code{\link{geo}}
#' @export 
extract_results <- function(method, response, full_results = TRUE, flatten = TRUE) {
  # NOTE - the geo() function takes the output of this function and renames the 
  # latitude and longitude columns
  
  NA_result <- get_na_value('lat', 'long', 1)
  
  # extract latitude and longitude as a dataframe
  lat_lng <- switch(method,
    'census' = response$result$addressMatches$coordinates[c('y','x')],
    'osm' = response[c('lat', 'lon')],
    'iq' = response[c('lat', 'lon')],
    'geocodio' = response$results$location[c('lat', 'lng')],
    'google' = response$results$geometry$location[c('lat','lng')],
    'opencage' = response$results$geometry[c('lat', 'lng')],
    'mapbox' <- data.frame(
      'lat' = response$features$center[[1]][2],
      'long' = response$features$center[[1]][1]
    ), # mapbox results are nested unnames lists
    'here' = response$items$position[c('lat','lng')],
    'tomtom' = response$results$position[c('lat', 'lon')],
    'mapquest' = response$results$locations[[1]]$latLng[c('lat','lng')]
  )
  
  # if null result then return NA
  if (length(lat_lng) == 0 ) return(NA_result)
  # check to make sure results aren't na or the wrong width
  if (nrow(lat_lng) == 0 | ncol(lat_lng) != 2) return(NA_result)
  
  # convert to numeric format
  lat_lng[, 1] <- as.numeric(as.character(lat_lng[, 1]))
  lat_lng[, 2] <- as.numeric(as.character(lat_lng[, 2]))
  
  if (full_results == TRUE) {
  # extract full results excluding latitude and longitude
  # note that lat/long are not excluded from the google results due to dataframe nesting
    results <- switch(method,
      'census' = response$result$addressMatches[!names(response$result$addressMatches) %in% c('coordinates')],
      'osm' = response[!names(response) %in% c('lat', 'lon')],
      'iq' =  response[!names(response) %in% c('lat', 'lon')],
      'geocodio' = response$results[!names(response$results) %in% c('location')],
      'google' = response$results,
      'opencage' = response$results[!names(response$results) %in% c('geometry')],
      'mapbox' = response$features,
      'here' = response$items,
      'tomtom' = response$results,
      'mapquest' = response$results$locations[[1]]
    )

    # add prefix to variable names that likely could be in our input dataset
    # to avoid variable name overlap
    for (var in c('address')) {
      if (var %in% names(results)) {
        names(results)[names(results) == var] <- paste0(method, '_', var)
      }
    }
    
    combined_results <- tibble::as_tibble(cbind(lat_lng, results))
  } else {
    combined_results <- lat_lng
  }
  
  combined_results <- tibble::as_tibble(combined_results)

  if (flatten == TRUE) return(jsonlite::flatten(combined_results))
  else return(combined_results)
}

#' Extract reverse geocoding results 
#' 
#' @description
#' Parses the output of the \code{\link{query_api}} function for reverse geoocding.
#' The address is extracted into the first column
#' of the returned dataframe. This function is not used for batch 
#' geocoded results. Refer to \code{\link{query_api}} for example
#' usage.
#' 
#' @param method method name
#' @param response  content from the geocoder service (returned by the \code{\link{query_api}} function)
#' @param full_results if TRUE then the full results (not just an address column)
#'   will be returned.
#' @param flatten if TRUE then flatten any nested dataframe content
#' @return geocoder results in tibble format 
#' @seealso \code{\link{get_api_query}} \code{\link{query_api}} \code{\link{reverse_geo}}
#' @export 
extract_reverse_results <- function(method, response, full_results = TRUE, flatten = TRUE) {
  # extract the single line address
  address <- switch(method,
                    'osm' = response['display_name'],
                    'iq' = response['display_name'],
                    'geocodio' = response$results['formatted_address'],
                    # take first row of multiple results with google for now
                    'google' = response$results[1, ]['formatted_address'],
                    'opencage' = response$results['formatted'],
                    'mapbox' = response$features['place_name'],
                    'here' = response$items['title'],
                    'tomtom' = response$addresses$address['freeformAddress'],
                    'mapquest' = format_address(response$results$locations[[1]],
                                                c('street', paste0('adminArea', seq(6, 1))))
  )
  
  # extract other results (besides single line address)
  if (full_results == TRUE) {
    results <- switch(method,
                      'osm' = cbind(response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))], 
                                    tibble::as_tibble(response[['address']]), tibble::tibble(boundingbox = list(response$boundingbox))),
                      'iq' =  cbind(response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))], 
                                    tibble::as_tibble(response[['address']]), tibble::tibble(boundingbox = list(response$boundingbox))),
                      'geocodio' = response$results[!names(response$results) %in% c('formatted_address')],
                      # take first row of multiple results with google for now
                      'google' = response$results[1, ][!names(response$results) %in% c('formatted_address')], 
                      'opencage' = response$results[!names(response$results) %in% c('formatted')],
                      'mapbox' = response$features[!names(response$features) %in% c('place_name')],
                      'here' = response$items[!names(response$items) %in% c('title')],
                      'tomtom' = response$addresses,
                      'mapquest' = response$results$locations[[1]]
    )
    
    # add prefix to variable names that likely could be in our input dataset
    # to avoid variable name overlap
    for (var in c('lat', 'lon', 'long', 'latitude', 'longitude', 'address')) {
      if (var %in% names(results)) {
      names(results)[names(results) == var] <- paste0(method, '_', var)
      }
    }
    combined_results <- dplyr::bind_cols(address, results)
    } else {
    combined_results <- address
  }
  
  combined_results <- tibble::as_tibble(combined_results)
  
  if (flatten == TRUE) return(jsonlite::flatten(combined_results))
  else return(combined_results)
}

# Extracts errors from a raw response object and display them
# expected response is query_api(...)$content (ie. the raw content from the HTTP request)
# This function is called in reverse_geo() and geo()
extract_errors_from_results <- function(method, response, verbose) {
  # test if response contains JSON content
  if (!jsonlite::validate(response)) {
    # tomtom does not return JSON content on errors 
    # in cases like this, display the raw content but limit the length
    # in case it is really long.
    message(paste0('Error: ', strtrim(as.character(response), 100)))
  }
  else {
    # parse JSON content
    raw_results <- jsonlite::fromJSON(response)
    
    # if results are blank
    if (length(raw_results) == 0) {
      if (verbose == TRUE) message("No results found")
    }
    else if ((method == 'osm') & ("error" %in% names(raw_results))) {
      message(paste0('Error: ', raw_results$error$message))
    }
    else if ((method == 'iq') & ("error" %in% names(raw_results))) {
      message(paste0('Error: ', raw_results$error))
    }
    else if ((method == 'mapbox') & (!is.data.frame(raw_results$features))) {
      if ("message" %in% names(raw_results)) {
        message(paste0('Error: ', raw_results$message))
      }
    }
    else if ((method == 'census') & ('errors' %in% names(raw_results))) {
      message(paste0('Error: ', raw_results$errors))
    }
    else if ((method == 'opencage') & (!is.data.frame(raw_results$results))) {
      if (!is.null(raw_results$status$message)) {
      message(paste0('Error: ', raw_results$status$message))
      }
    }
    else if ((method == 'geocodio') & (!is.data.frame(raw_results$results))) {
      if ("error" %in% names(raw_results)) {
       message(paste0('Error: ', raw_results$error))
      }
    }
    else if ((method == 'google') & (!is.data.frame(raw_results$results))) {
      if ("error_message" %in% names(raw_results)) {
        message(paste0('Error: ', raw_results$error_message))
      }
    }
    else if ((method == 'here') & (!is.data.frame(raw_results$items))) {
      if ("error_description" %in% names(raw_results)) message(paste0('Error: ', raw_results$error_description))
      else if ("title" %in% names(raw_results)) message(paste0('Error: ', raw_results$title))
    } 
    else if ((method == 'tomtom') & (!is.data.frame(raw_results$addresses)) & (!is.data.frame(raw_results$results))){
      if ('errorText' %in% names(raw_results)) {
        message(paste0('Error: ', raw_results$errorText))
      }
      else if ('error' %in% names(raw_results)) {
        message(paste0('Error: ', raw_results$error))
      }
    }
    else if (method == 'mapquest'){
      if (!is.null(raw_results$info$messages)) message(paste0('Error: ', raw_results$info$messages))
    }
    
  }
}

# For a list of dataframes, creates an NA df with 1 row with the column name supplied
# this is used in parsing the response of the geocodio batch geocoder
filler_df <- function(x, column_names) {
  if (length(x) == 0) {
    filler_df <- data.frame(row.names = 1)

    for (col_name in column_names) {
      filler_df[col_name] <- NA
    }
    return(filler_df)
    
  } else return(x)
}

# Used by batch census function
# input is a single character value. 
# output is an unnamed numeric list with 2 elements: lat, long
# if comma contained in input then split it. otherwise return NA list
split_coords <- function(input) {
  if (grepl(',', input, fixed = TRUE)) {
    split <- as.list(unlist(strsplit(input, "," , fixed = TRUE)))
  }
  else split <- (list('', ''))
  
  return(as.numeric(split))
}

# Return a 2 column, 1 row NA tibble dataframe for coordinates that aren't found
# Given the column names (as strings)
get_na_value <- function(lat, long, rows = 1) {
  NA_df <- tibble::tibble(a = rep(as.numeric(NA), rows), b = rep(as.numeric(NA), rows))
  colnames(NA_df) <- c(lat, long)
  return(NA_df)
}


# remove a literal double quote from a string
# used with NSE
rm_quote <- function(string) gsub("\"","", string)

# How many seconds have elapsed since start time t0 (as defined by a t0 <- Sys.time() call) 
get_seconds_elapsed <- function(t0) {
  return(as.numeric(difftime(Sys.time(), t0, units = 'secs')))
}

# print time
print_time <- function(text, num_seconds) {
  message(paste0(text, ': ', round(num_seconds, 1), ' seconds'))
}

# Use Sys.sleep() to pause until a certain amount of time has elapsed
pause_until <- function(start_time, min_time, debug=FALSE) {
  ## Make sure the proper amount of time has elapsed for the query per min_time
  seconds_elapsed <- get_seconds_elapsed(start_time)
  if (debug == TRUE) print_time("Query completed in", seconds_elapsed)
  
  # Sleep if necessary to make query take the minimum amount of time
  if (seconds_elapsed < min_time) {
    Sys.sleep(min_time - seconds_elapsed)
    total_time_elapsed <- get_seconds_elapsed(start_time)
    if (debug == TRUE) print_time("Total query time (including sleep)", total_time_elapsed)
  }
}

# Used for mapquest - provide formatted address based on fields
# Could be extended to other providers if no frmt.address is provided - non specific
# input is a data.frame/tibble and the list of fields used for creating 
# a formatted address
# output is a tibble with the formatted address
# formatted address follow the order of fields vector
# Result sample:
# # A tibble: 1 x 1
# formatted_address          
# <chr>                      
# 1 ES, 2 Calle de Espoz y Mina
format_address <- function(df, fields) {
  frmt_df <- tibble::as_tibble(df)
  
  col_order <- intersect(fields, names(frmt_df))
  
  frmt_df <- dplyr::relocate(frmt_df[col_order], col_order)
  
  frmt_char <- as.character(apply(frmt_df, 1, function(x) {
    y <- unique(as.character(x))
    y <- y[!y %in% c('', 'NA')]
    paste0(y, collapse = ', ')
  }))
  
  frmt_char[frmt_char == 'NA'] <- NA
  frmt_out <- tibble::tibble(formatted_address = frmt_char)
  return(frmt_out)
}
