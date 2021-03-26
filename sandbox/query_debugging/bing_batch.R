limit <- 1
custom_query <- list()
verbose <- TRUE
timeout <- 20
lat <- "latit"
long <- "longit"
api_url <- NULL

address_df <- tibble::tribble(~address, "Madrid", "hahuauhauauhu", "Torre Eiffel, Paris")
#address_df <- tibble::tibble(address = mapSpain::esp_munic.sf[1:101,]$name)

#Bing-----------------
#https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/

# Specific endpoint
if (is.null(api_url)) api_url <- 'http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode'

#address_df <- unique_addresses[names(unique_addresses) %in% get_generic_parameters('bing', address_only = TRUE)]

# filler result to return if needed
NA_batch <- get_na_value(lat, long, rows = nrow(address_df))

# Construct query ----
# Bing needs a special list of params
# https://docs.microsoft.com/es-es/bingmaps/spatial-data-services/geocode-dataflow-api/

query_parameters <- get_api_query('bing',
                                  list(api_key = get_key('bing')),
                                  custom_parameters = list(input = 'pipe')
)
if (verbose == TRUE) display_query(api_url, query_parameters)

# Create body of the POST request----
# Needs to have Id and GeocodeRequest/Query  
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

address_df <- dplyr::bind_cols(Id = seq_len(nrow(address_df)), address_df)

# Create mock cols
mock <- as.data.frame(matrix(ncol=length(response_fields), nrow = nrow(address_df))
)
address_body <- dplyr::bind_cols(address_df, mock)

# Rename
names(address_body) <- c("Id","GeocodeRequest/Query", response_fields)

# Create file
body_file <- tempfile()
temp
cat("Bing Spatial Data Services, 2.0", file=body_file, append=FALSE, sep = "\n")
cat(paste0(names(address_body), collapse = "|"), file=body_file, append=TRUE, sep = "\n")
for (j in (seq_len(nrow(address_body)))){
  body <- paste0(address_body[j, ], collapse = "|")
  body <- gsub('NA','',body)
  cat(body, file=body_file, append=TRUE, sep = "\n")
}

cat

# Step 1: Run job and retrieve id ----
# Modification from query_api function
if (verbose) message('\nBing: Batch job:')
httr::up
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
if (verbose) httr::message_for_status(job)

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

if (verbose) {
  status_results <- status_get$resourceSets[[1]]$resources[[1]]
  message(paste0('Bing: Processed: ', status_results$processedEntityCount,
                 " | Failed: ", status_results$failedEntityCount))
}

update_time_elapsed <- get_seconds_elapsed(init_process)

if (verbose) print_time('Bing: Batch job processed in', update_time_elapsed)


# Step 3: GET results and parse ----
links <- status_get$resourceSets[[1]]$resources[[1]]$links

# If not succeeded return NA
if (links[[2]]$name != 'succeeded'){
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
# Merge to original addresses and output
result_parsed$Id
base <- tibble::as_tibble(address_body)
results <- merge(base[c("Id")], 
                 result_parsed, 
                 all.x = TRUE)

names(results)[names(results) == 'GeocodeResponse.Point.Latitude'] <- lat
names(results)[names(results) == 'GeocodeResponse.Point.Longitude'] <- long

results

if (full_results == FALSE) return(results[c(lat, long)])
else return(cbind(results[c(lat, long)], results[!names(results) %in% c(lat, long)]))





content <- httr::content(job)

httr::content(job)
query_parameters

httr::message_for_status(job)
body
api_url

# Live test -----
library(tibble)
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "bing",
  lat = "latt",
  long = "longgg",
  mode = 'batch',
  verbose = TRUE
)


tidygeocoder::geo(
  address = c("Plaza Mayor", "xxxxxxxxx", "George Street"),
  method = "bing",
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  mode = 'batch',
  verbose = TRUE
)



# Silent
tidygeocoder::geo(
  address = c("Plaza Mayor, Madrid", "George Street, Edinburgh"),
  method = "bing",
  limit = 1,
  full_results = TRUE,
  return_addresses = TRUE,
  mode = 'batch',
  verbose = FALSE
)

# Try single result

tidygeocoder::geo(
  address = c("Plaza Mayor, Madrid, Spain"),
  method = "bing",
  mode = "batch",
  full_results = TRUE,
  lat = "latt",
  long = "longgg",
  verbose = TRUE
)

# Error for limit

library(mapSpain)
library(tibble)
library(dplyr)

address <- tibble(direcciones = mapSpain::esp_munic.sf$name) %>%
  slice(1:51)

err <- address %>%
  geocode(
    address = "direcciones", method = "bing", full_results = TRUE,
    mode = 'batch',
    verbose = TRUE, lat = "latitude"
  )


err
# Error on empty results
tidygeocoder::geo(
  address = c("aaaaaaaaaaaa", "-------"),
  method = "bing",
  limit = 1,
  full_results = TRUE,
  return_addresses = TRUE,
  verbose = TRUE,
  mode = 'batch',
  custom_query = list(thumbMaps ="xxxx")
)

# Full batch test
address_ok <- tibble(direcciones = mapSpain::esp_munic.sf$name) %>%
  slice(1:50)

full_batch <- address_ok %>%
  geocode(
    address = "direcciones", method = "bing", full_results = TRUE,
    mode = 'batch',
    verbose = TRUE, lat = "latitude"
  )

full_batch
