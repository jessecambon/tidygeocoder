### Put common utility functions here


# utility function for extracting setting values according to the method column
get_setting_value <- function(df, method, value_column) {
  return(
    df[which(df[['method']] == method), ][[value_column]]
  )
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
pause_until <- function(start_time, min_time, debug = FALSE) {
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


# QA Checks --------------------------------------------------------------------------------------------------------------
# functions called by reverse_geo() and/or geo()

check_api_options <- function(api_options, func_name) {
  for (param in names(api_options)) {
    if (!param %in% c('cascade_flag', "init", names(pkg.globals$default_api_options))) {
      stop(paste0("Invalid parameter ", '"', param, '"', " used in the api_options argument. See ?", func_name), call. = FALSE)
    }
  }
}

# check the data type of an address argument - called by geo() function
# should not be a matrix, class, or dataframe for instance
# allow factor since it could be coerced to a datatype by address handler function
# allow numeric for zip codes etc.
check_address_argument_datatype <- function(arg, arg_name) {
  if (!(is.null(arg) || is.character(arg) || is.numeric(arg) || is.na(arg) || is.factor(arg))) {
    stop(paste0('Improper datatype for ', arg_name, '. See ?geo'), call. = FALSE)
  }
}

check_verbose_quiet <- function(verbose, quiet, reverse) {
  input_terms <- get_coord_address_terms(reverse)
  
  if (quiet == TRUE && verbose == TRUE) {
    stop(paste0("quiet and verbose cannot both be TRUE. See ?", input_terms$base_func_name))
  }
}

# check that method argument is valid
check_method <- function(method, reverse, mode, batch_funcs, cascade_order = list()) {
  input_terms <- get_coord_address_terms(reverse)
  
  # all possible methods
  method_services <- unique(tidygeocoder::api_parameter_reference[['method']])
  
  # legal batch methods
  batch_methods <- names(batch_funcs)
  
  # which methods are legal for single input queries
  single_input_methods <- if (reverse == FALSE) {
    c('cascade', method_services)
  } else {
  # remove methods that don't have a reverse mode (currently only 'census')
  method_services[!method_services %in% pkg.globals$no_reverse_methods]
  }
  
  # for geo() check cascade_order
  if (reverse == FALSE) {
    if (!(cascade_order[1] %in% method_services) || !(cascade_order[2] %in% method_services) || (length(cascade_order) != 2) || !(is.character(cascade_order))) {
      stop('Invalid cascade_order argument. See ?geo', call. = FALSE)
    }
    if (method == 'cascade' && mode == 'batch' && (length(intersect(cascade_order, names(batch_funcs)) != 2))) {
      stop("To use method = 'cascade' and mode = 'batch', both methods specified in cascade_order
          must have batch geocoding capabilities. See ?geo", call. = FALSE)
    }
  }
    
  if (mode == 'batch' && (!method %in% batch_methods)) {
    stop(paste0('The "', method, '" method does not have a batch', 
      if (reverse == TRUE) " reverse" else "",
      ' geocoding function. See ?', input_terms$base_func_name) , call. = FALSE)
  }
    
  if (!(method %in% single_input_methods)) {
    stop(paste0('Invalid method argument. See ?', input_terms$base_func_name), call. = FALSE)
  } 
}
 
# check some arguments common to geo() and reverse_geo()
# fun_name is the name of the function that calls this one
check_common_args <- function(fun_name, mode, limit, batch_limit, min_time) {
  
  if (!(mode %in% c('', 'single', 'batch'))) {
    stop(paste0('Invalid mode argument. See ?', fun_name), call. = FALSE)
  }

  # limit should either be NULL or numeric and >= 1
  if (!(is.null(limit) || (is.numeric(limit) && limit >= 1))) {
    stop(paste0('limit must be NULL or >= 1. See ?', fun_name), call. = FALSE)
  }

  # batch_limit should either be NULL or numeric and >= 1
  if (!(is.null(batch_limit) || (is.numeric(batch_limit) && batch_limit >= 1))) {
    stop(paste0('batch_limit must be NULL or >= 1. See ?', fun_name), call. = FALSE)
  }
  
  # min_time should either be NULL or numeric and >= 0
  if (!(is.null(min_time) || (is.numeric(min_time) && min_time >= 0))) {
    stop(paste0('min_time must be NULL or >= 0. See ?', fun_name), call. = FALSE)
  }  
}

# This check prevents a address-results misalignment issue https://github.com/jessecambon/tidygeocoder/issues/88
# used in geocode() and reverse_geocode()
check_limit_return_input <- function(limit, return_input) {
  if ((is.null(limit) || limit != 1) && return_input == TRUE) {
    stop('To use limit > 1 or limit = NULL, set return_input to FALSE.', call. = FALSE)
  }
}

# check for conflict between limit and return_coords/return_addresses argument in reverse_geo() and geo() 
# return_input = return_coords (or return_addresses
check_limit_for_batch <- function(limit, return_input, reverse) {
  
  input_terms <- get_coord_address_terms(reverse)
  
  if ((is.null(limit) || limit != 1) && return_input == TRUE) {
    stop(paste0('For batch geocoding (more than one ', input_terms$input_singular, 
    ' per query) the limit argument must
    be 1 (the default) OR the ', input_terms$return_arg, ' argument must be FALSE. Possible solutions:
    1) Set the mode argument to "single" to force single (not batch) geocoding 
    2) Set limit argument to 1 (ie. 1 result is returned per ',  input_terms$input_singular, ')
    3) Set ', input_terms$return_arg, ' to FALSE
    See ?', input_terms$base_func_name, ' for details.'),
    call. = FALSE)
  }
}


# check for HERE method batch queries --- for use in geo() and reverse_geo()
check_here_return_input <- function(here_request_id, return_input, reverse) {
  
  input_terms <- get_coord_address_terms(reverse)

  # If a previous job is requested return_addresses should be FALSE
  # This is because the job won't send the addresses, but would recover the
  # results of a previous request
  if (is.character(here_request_id) && return_input == TRUE) {
    stop('HERE: When requesting a previous job via here_request_id, set ', input_terms$return_arg,
    ' to FALSE. See ?', input_terms$base_func_name, ' for details.', call. = FALSE)
  }
}

# Misc -----------------------------------------------------------------------------------------

## function for extracting everything except the single line 
## address from the reverse geocoding results of osm and iq
extract_osm_reverse_full <- function(response) {
  a <- response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))]
  a[sapply(a, function(x) length(x) == 0)] <- NULL # get rid of empty lists
  b <- tibble::as_tibble(response[['address']])
  c <- tibble::tibble(boundingbox = list(response$boundingbox))
  
  return(
    tibble::as_tibble(dplyr::bind_cols(as.data.frame(a), b, c))
  )
}

# note issue #112: https://github.com/jessecambon/tidygeocoder/issues/112
extract_bing_latlng <- function(response) {
  
  # if no rows are found then return an empty data frame so NA results will be returned
  if (length(response$resourceSets$resources[[1]]) == 0) return(data.frame())
  
  # otherwise extract the latitude and longitude
  latlng <- as.data.frame(matrix(unlist(response$resourceSets$resources[[1]]$point$coordinates), 
                                 ncol = 2, byrow = TRUE), col.names = c('lat', 'long'))
  
  return(latlng)
}


## Progress bars -----------------------------------------------------------------------------

# Conditions for displaying a progress bar
# For consistency/continuity, these are the same conditions 
# that are used for the {readr} package
show_progress_bar <- function() {
    getOption("tidygeocoder.progress_bar", TRUE) && # option is TRUE or not found
    interactive() && # interactive session
    !isTRUE(getOption("rstudio.notebook.executing")) && # Not running in an RStudio notebook chunk
    !isTRUE(getOption("knitr.in.progress")) # Not actively knitting a document
}


# create a progress bar using the {progress} package
# format_text formats the progress bar and total_count is the total 
# number of iterations for the progress bar (number of addresses or coordinates)
create_progress_bar <- function(
  total_count,
  format_text = "[:bar] :current/:total (:percent) Elapsed: :elapsed Remaining: :eta"
  ) {
  pb <- progress::progress_bar$new(
    format = format_text,
    clear = FALSE,
    total = total_count,
    show_after = 0
  )
  
  pb$tick(0) # start progress bar
  
  return(pb)
}

# Create a message to tell user how many addresses/coordinates are getting sent
# in a batch query and to what geocoding service
# reverse = TRUE for reverse geocoding
query_start_message <- function(method, num_inputs, reverse, batch, display_time = FALSE) {
  input_terms <- get_coord_address_terms(reverse)
  
  message(paste0('Passing ', 
                 format(num_inputs, big.mark = ','), ' ', 
                 if (num_inputs == 1) input_terms$input_singular else input_terms$input_plural,
                 ' to the ', 
                 # get proper name of the service
                 get_setting_value(tidygeocoder::api_info_reference, method, 'method_display_name'), ' ',
                 if (batch == TRUE) 'batch' else paste0('single ', input_terms$input_singular),
                 ' geocoder', 
                 # display time when query was sent (used for batch queries)
                 if (display_time == TRUE) paste0(" - ", format(Sys.time(), "%I:%M %p")) else "")
          )
}


query_complete_message <- function(start_time) {
  print_time("Query completed in", get_seconds_elapsed(start_time))
}

# Misc --------------------------------

# if necessary, modify the API URL - called by geo() and reverse_geo()
# returns the API URL
# reverse indicates if query is reverse geocoding or forward geocoding
api_url_modification <- function(method, api_url, generic_query, custom_query, reverse) {

  # Workaround for Mapbox/TomTom - The search_text should be in the API URL
  if (method %in% c('mapbox', 'tomtom')) {
    api_url <- if (reverse == TRUE) {
      paste0(api_url, custom_query[["to_url"]], ".json")
    } else {
      gsub(" ", "%20", paste0(api_url, generic_query[['address']], ".json"))      
    }
    # Remove semicolons (Reserved for batch)
    api_url <- gsub(";", ",", api_url)
  }
  
  return(api_url)
}

# apply api options defaults for options not specified by the user.
# called by geo() and reverse_geo()
apply_api_options_defaults <- function(api_options) {
  for (name in names(pkg.globals$default_api_options)) {
    if (is.null(api_options[[name]])) api_options[[name]] <- pkg.globals$default_api_options[[name]]
  }
  return(api_options)
}

# Set the api_options[["init"]] parameter
# init is for internal package use only, used to designate if the geo() or reverse_geo() function
# is being called for the first time (init = TRUE) or if it has called itself 
# recursively (init = FALSE)
initialize_init <- function(api_options) {
  if (is.null(api_options[["init"]])) {
    api_options[["init"]] <- TRUE
  }
  return(api_options)
}