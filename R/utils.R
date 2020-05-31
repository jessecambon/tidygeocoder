## Put common utilities here

# How many seconds have elapsed since start time t0 (as defined by a t0 <- Sys.time() call) 
get_seconds_elapsed <- function(t0) {
  return(as.numeric(difftime(Sys.time(), t0, units = 'secs')))
}

# print time
print_time <- function(text, num_seconds) {
  message(paste0(text, ': ', round(num_seconds,1),' seconds'))
}


# Use Sys.sleep() to pause until a certain amount of time has elapsed
pause_until <- function(start_time,min_time,debug=FALSE) {
  ## Make sure the proper amount of time has elapsed for the query per min_time
  seconds_elapsed <- get_seconds_elapsed(start_time)
  if (debug == TRUE) print_time("Time elapsed", seconds_elapsed)
  
  # Sleep if necessary to make query take the minimum amount of time
  if (seconds_elapsed < min_time) {
    Sys.sleep(min_time - seconds_elapsed)
    if (debug == TRUE) print_time("Time elapsed (after sleep)", get_seconds_elapsed(start_time))
  }
}

# Extract latitude, longitude as as numeric vector c(lat, long)
# response is the parsed json response
extract_coords <- function(method, response) {
  if (method == 'census') {
    coord_xy <- response$result$addressMatches$coordinates[1,]
    lat_lng <- c(coord_xy$y, coord_xy$x)
  } else if (method == 'osm') {
    lat_lng <- as.numeric( c(response$lat, response$lon) )
  }
  # Convert to a 1 row 2 column tibble and return
  return(lat_lng)
}  

# split single coordinate set
split_coords <- function(input) as.numeric(unlist(strsplit(input,"\\,")))

#### Split c(lat,long) coordinates into a tibble with 1 row and 2 columns   
#split_coordinates <- function(coords) tibble::tibble(lat = coords[[1]] ,long = coords[[2]])

#### Split list of coordinates into a dataframe with two columns and one row per coordinate
#unnest_coordinates <- function(coordinate_list) do.call('rbind',lapply(coordinate_list, split_coordinates))

### Return a 2 column, 1 row NA tibble dataframe for coordinates that aren't found
# Given the column names (as strings)
get_na_value <- function(lat, long) {
  NA_df <- tibble::tribble(
    ~a, ~b,
    NA, NA
  )
  colnames(NA_df) <- c(lat, long)
  return(NA_df)
}

# For a list of dataframes, creates an NA df with 1 row with the column name supplied
filler_df <- function(x, column_names) {
  if (nrow(x) == 0) {
    filler_df <- data.frame(row.names = 1)

    for (col_name in column_names) {
      filler_df[col_name] <- NA
    }
    return(filler_df)
    
  } else return(x)
}


# Quoted unquoted vars without double quoting quoted vars
#nse_eval <- function(x) gsub("\"","", deparse(substitute(x)))


