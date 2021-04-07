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