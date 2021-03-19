limit <- 1
custom_query <- list(keya="aa")
verbose <- TRUE
timeout <- 20
api_url <- "http://www.mapquestapi.com/geocoding/v1/batch"
address <- "addr"
lat <- runif(100, 38, 42)
long <- runif(100, -6, 1)
mapquest_open = FALSE
full_results = TRUE
# white house, toronto, junk lat/lng
lat <- c(38.89586, 300, 43.6534817)
long <- c(-77.0307713, 500, -79.3839347)



latLng <- as.character(paste0(lat, ',', long))

NA_value <- get_na_value(address, "xxx", rows = length(latLng))[address] # filler result to return if needed

# Construct query
# Depends if single or multiple query

# Single: Now allowed on batch, return a single query ----
if (length(latLng) == 1) {
  results <- reverse_geo(lat = lat, long = long, mode = 'single', method = 'mapquest',
                         full_results = full_results, custom_query = custom_query, 
                         verbose = verbose, api_url = api_url, limit = limit, 
                         mapquest_open = mapquest_open)
  
  
  # rename lat/long columns
  names(results)[names(results) == 'address'] <- address
  
  return(results[!names(results) %in% c('lat', 'long')])
}

# Multiple via POST ----
# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
if (is.null(api_url)) {
  url_domain <- if (mapquest_open) "http://open" else  "http://www"
  
  api_url <- paste0(url_domain, ".mapquestapi.com/geocoding/v1/batch")
}

# Construct query - for display only

query_parameters <- get_api_query("mapquest", 
                                  list(limit = limit, api_key = get_key("mapquest")),
                                  custom_parameters = custom_query)

if (verbose == TRUE) display_query(api_url, query_parameters)

# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
# Construct POST query

# A. Only certain parameters should be in the POST call----

body_params <- query_parameters[!names(query_parameters) %in% c("key", "callback")]
query_parameters <- query_parameters[names(query_parameters) %in% c("key", "callback")]

# B. Construct Body----
coords_list <- list(
  locations = latLng,
  options = body_params
)

## Query API ----

query_results <- query_api(api_url, query_parameters, mode = "list",
                           input_list = coords_list, timeout = timeout)

# C. Error handling----
# Parse result code
if (jsonlite::validate(query_results$content)) {
  status_code <- jsonlite::fromJSON(query_results$content, flatten = TRUE)$info$statuscode
} else {
  status_code <- query_results$status
}
# Successful status_code is 0
if (status_code == "0") status_code <- "200"
status_code <- as.character(status_code)

if (verbose == TRUE) message(paste0("HTTP Status Code: ", as.character(status_code)))

## Extract results -----------------------------------------------------------------------------------
# if there were problems with the results then return NA
if (status_code != "200") {
  if (!jsonlite::validate(query_results$content)) {
    # in cases like this, display the raw content but limit the length
    # in case it is really long.
    message(paste0("Error: ", strtrim(as.character(query_results$content), 100)))
  } else {
    # Parse and get message
    content <- jsonlite::fromJSON(query_results$content, flatten = TRUE)
    if (!is.null(content$info$messages)) message(paste0("Error: ", content$info$messages))
  }
  # Return empty and exit
  return(NA_value)
}
# D. On valid API response-----

# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(query_results$content, flatten = TRUE)

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
result_list <- content$results$locations
result_list_filled <- lapply(result_list, filler_df, c('street'))
results <- dplyr::bind_rows(result_list_filled)

# Format address
frmt_address <- format_address(results, c('street', paste0('adminArea', seq(6, 1))))
results <- tibble::as_tibble(cbind(frmt_address, results))

names(results)[names(results) == 'formatted_address'] <- address

if (full_results == FALSE)  return(results[address])
else return(cbind(results[address], results[!names(results) %in% c(address)]))

# Live tests----

tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "mapquest",
  verbose = TRUE,
  limit = 1
)




tidygeocoder::reverse_geo(
  lat = c(38.89586, 300, 43.6534817),
  long = c(-77.0307713, 500, -79.3839347),
  method = "mapquest",
  address = "aaa",
  verbose = TRUE,
  mode = "batch",
  return_coords = FALSE,
  full_results = TRUE,
  limit = 1
)

# Single on batch
tidygeocoder::reverse_geo(
  lat = c(38.89586),
  long = c(-77.0307713),
  method = "mapquest",
  mode = "batch",
  address = "abcde",
  verbose = TRUE,
  full_results = TRUE,
  limit = 1
)

tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "mapquest",
  address = "direccion",
  verbose = TRUE,
  full_results = TRUE,
  return_coords = FALSE,
  limit = 4
)




test3 <- tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "mapquest",
  verbose = TRUE,
  full_results = TRUE,
  limit = 1,
  custom_query = list(
    language = "de-DE"
  )
)

tibble::glimpse(test3)



test5 <- tidygeocoder::reverse_geo(
  lat = c(48.8582, 40.45305),
  long = c(2.2944, -3.68834),
  method = "mapquest",
  verbose = FALSE,
  full_results = TRUE,
  return_coords = FALSE,
  limit = 30,
  custom_query = list(
    thumbMaps  = FALSE
  )
)
glimpse(test5)

tidygeocoder::reverse_geo(
  lat = c(48.8582, 40.45305),
  long = c(2.2944, -3.68834),
  method = "mapquest",
  no_query = TRUE
)

# Bulk query----


bulk <- tidygeocoder::reverse_geo(
  lat = runif(100, 38, 42),
  long = runif(100, -6, 1),
  method = "mapquest",
  verbose = TRUE
)

bulk

# Try errors

tidygeocoder::reverse_geo(
  lat = runif(101, 38, 42),
  long = runif(101, -6, 1),
  method = "mapquest",
  address = "SHNS",
  verbose = TRUE
)


tidygeocoder::reverse_geo(
  lat = runif(3, 38, 42),
  long = runif(3, -6, 1),
  method = "mapquest",
  address = "SHNS",
  verbose = TRUE,
  custom_query = list(key="s")
)

tidygeocoder::reverse_geo(
  lat = runif(3, 38, 42),
  long = runif(3, -6, 1),
  method = "mapquest",
  address = "SHNS",
  verbose = TRUE,
  custom_query = list(thumbMaps="s")
)
