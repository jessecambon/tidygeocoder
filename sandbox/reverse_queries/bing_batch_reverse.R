limit <- 1
custom_query <- list(keya="aa")
verbose <- TRUE
timeout <- 20
api_url <- NULL
address <- "addr"
lat <- runif(100, 38, 42)
long <- runif(100, -6, 1)
full_results = TRUE
# white house, toronto, junk lat/lng
lat <- c(38.89586, 300, 43.6534817)
long <- c(-77.0307713, 500, -79.3839347)

# From here...----

# Specific endpoint
if (is.null(api_url)) api_url <- 'http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode'


latlon_df <- tibble::tibble(Id = seq_len(length(lat)),
                            Latitude = lat,
                            Longitude = long)
names(latlon_df) <- c("Id", "ReverseGeocodeRequest/Location/Latitude", 
                      "ReverseGeocodeRequest/Location/Longitude")

# filler result to return if needed
NA_batch <- get_na_value(address, "xxx", rows = nrow(latlon_df))[address]

# Construct query ----
# Bing needs a special list of params
# https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/
query_parameters <- get_api_query('bing',
                                  list(api_key = get_key('bing')),
                                  custom_parameters = list(input = 'pipe')
)
if (verbose == TRUE) display_query(api_url, query_parameters)

# Create body of the POST request----
# Needs to have Id and some fields, already on latlon_df
# Also needs to add response fields
response_fields <- c('GeocodeResponse/Address/AddressLine',
                     'GeocodeResponse/Address/AdminDistrict',
                     'GeocodeResponse/Address/CountryRegion',
                     'GeocodeResponse/Address/AdminDistrict2',
                     'GeocodeResponse/Address/FormattedAddress',  
                     'GeocodeResponse/Address/Locality',
                     'GeocodeResponse/Address/PostalCode',
                     'GeocodeResponse/Address/PostalTown',
                     'GeocodeResponse/Address/Neighborhood',
                     'GeocodeResponse/Address/Landmark',
                     'GeocodeResponse/Confidence',
                     'GeocodeResponse/Name',
                     'GeocodeResponse/EntityType',
                     'GeocodeResponse/MatchCodes',
                     'GeocodeResponse/Point/Latitude',
                     'GeocodeResponse/Point/Longitude',
                     'GeocodeResponse/BoundingBox/SouthLatitude',
                     'GeocodeResponse/BoundingBox/WestLongitude',
                     'GeocodeResponse/BoundingBox/NorthLatitude',
                     'GeocodeResponse/BoundingBox/EastLongitude')  

# Create mock cols
mock <- as.data.frame(
  matrix(data="", ncol=length(response_fields), nrow = nrow(latlon_df))
)
names(mock) <- response_fields

# Create tibble for body
latlon_body <- dplyr::bind_cols(latlon_df, mock)

# Create file
body_file <- tempfile()
cat("Bing Spatial Data Services, 2.0", file=body_file, append=FALSE, sep = "\n")
cat(paste0(names(latlon_body), collapse = "|"), file=body_file, append=TRUE, sep = "\n")
for (j in (seq_len(nrow(latlon_body)))){
  body <- paste0(latlon_body[j, ], collapse = "|")
  body <- gsub('NA','',body)
  cat(body, file=body_file, append=TRUE, sep = "\n")
}
# Body created on body_file

# Step 1: Run job and retrieve id ----
# Modification from query_api function
if (verbose) message('\nBing: Batch job:')

# Batch timer
init_process <- Sys.time()
job <- httr::POST(api_url,
                  query = query_parameters,
                  body = httr::upload_file(body_file),
                  httr::timeout(60 * timeout)
)
httr::warn_for_status(job)
status_code <- httr::status_code(job)
job_result <- httr::content(job)

# On error return NA
if (status_code != '201'){
  if (verbose) message(paste0("Error: ", job_result$errorDetails))  
  return(NA_batch)
}

jobID <- job_result$resourceSets[[1]]$resources[[1]]$id

# Step 2: Check job until is done ----
if (verbose) {
  httr::message_for_status(job)
  # Force new line
  message()
}

current_status <- ''

while (current_status %in%  c('Pending', '')) {
  Sys.sleep(3) # Arbitrary, 3sec
  status <- httr::GET(url = paste0(api_url, '/', jobID),
                      query = list(key = get_key('bing'))
  )
  status_get <- httr::content(status)
  
  prev_status <- current_status
  current_status <- status_get$resourceSets[[1]]$resources[[1]]$status
  
  if (verbose && prev_status != current_status){
    message(paste0("Bing: ",current_status))
  }
}

status_results <- status_get$resourceSets[[1]]$resources[[1]]
process <- as.integer(status_results$processedEntityCount)
errors <- as.integer(status_results$failedEntityCount)
succees <- process - errors

if (verbose) {
  httr::message_for_status(job)
  # Force new line
  message()
  message(paste0('Bing: Processed: ', process,
                 " | Succeeded: ", succees,
                 " | Failed: ", errors))
}

update_time_elapsed <- get_seconds_elapsed(init_process)

if (verbose) print_time('Bing: Batch job processed in', update_time_elapsed)


# Step 3: GET results and parse ----
links <- status_get$resourceSets[[1]]$resources[[1]]$links

# If not succeeded return NA
if (process == errors){
  if (verbose) message("Bing: All failed")
  return(NA_batch)
}

# Download and parse succeeded results

batch_results <-
  httr::GET(url = links[[2]]$url,
            query = list(key = get_key('bing'))
  )

result_content <- httr::content(batch_results, as = 'text', encoding = 'UTF-8')

# Skip first line
result_parsed <- tibble::as_tibble(utils::read.table(text = result_content,
                                                     skip = 1,
                                                     header = TRUE, 
                                                     sep = "|"))
# Merge to original latlons and output----
base <- tibble::as_tibble(latlon_df)
results <- merge(base["Id"], 
                 result_parsed, 
                 all.x = TRUE)

names(results)[names(results) == 'GeocodeResponse.Address.FormattedAddress'] <- address

if (full_results == FALSE) return(results[address])
else return(cbind(results[address], results[!names(results) %in% c(address)]))

results
# Live tests----

tidygeocoder::reverse_geo(
  lat = c(48.858296, 40.4530541),
  long = c(2.294479, -3.6883445),
  method = "bing",
  mode = "batch",
  verbose = TRUE,
  limit = 1
)




tidygeocoder::reverse_geo(
  lat = c(38.89586, 300, 43.6534817),
  long = c(-77.0307713, 500, -79.3839347),
  method = "bing",
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
  method = "bing",
  mode = "batch",
  address = "abcde",
  verbose = TRUE,
  full_results = TRUE,
  limit = 1
)


tidygeocoder::reverse_geo(
  lat = c(48.8582, 40.45305),
  long = c(2.2944, -3.68834),
  method = "bing",
  no_query = TRUE
)

# Bulk query----


bulk <- tidygeocoder::reverse_geo(
  lat = runif(50, 38, 42),
  long = runif(50, -6, 1),
  method = "bing",
  mode = "batch",
  verbose = TRUE
)

bulk
# Test that bulk only when enforced
tidygeocoder::reverse_geo(
  lat = runif(2, 38, 42),
  long = runif(2, -6, 1),
  method = "bing",
  verbose = TRUE
)

# Error for limit----

tidygeocoder::reverse_geo(
  lat = runif(51, 38, 42),
  long = runif(51, -6, 1),
  method = "bing",
  mode = "batch",
  verbose = TRUE
)

# Error on empty
tidygeocoder::reverse_geo(
  lat = runif(2, 300, 400),
  long = runif(2, -6, 1),
  method = "bing",
  mode = "batch",
  verbose = TRUE
)
