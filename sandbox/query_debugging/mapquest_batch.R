limit <- 1
custom_query <- list(keya="aa")
verbose <- TRUE
timeout <- 20
lat <- "latit"
long <- "longit"
api_url <- "http://www.mapquestapi.com/geocoding/v1/batch"

address_df <- tibble::tribble(~address, "Madrid, ES", "hahuauhauauhu", "Segovia")
#address_df <- tibble::tibble(address = mapSpain::esp_munic.sf[1:101,]$name)

if (is.null(api_url)) api_url <- "http://www.mapquestapi.com/geocoding/v1/batch"


NA_value <- get_na_value(lat, long, rows = nrow(address_df)) # filler result to return if needed

# Construct query - for display only

query_parameters <- get_api_query("mapquest", list(limit = limit, api_key = get_key("mapquest")),
                                  custom_parameters = custom_query
)
if (verbose == TRUE) display_query(api_url, query_parameters)


# https://developer.mapquest.com/documentation/geocoding-api/batch/post/
# Construct POST query

# A. Only certain parameters should be in the POST call----

body_params <- query_parameters[!names(query_parameters) %in% c("key", "callback")]
query_parameters <- query_parameters[names(query_parameters) %in% c("key", "callback")]

# B. Construct Body----
address_list <- list(
  locations = address_df[["address"]],
  options = body_params
)

# Query API

query_results <- query_api(api_url, query_parameters, mode = "list", input_list = address_list, timeout = timeout)

# Error handling----
# Parse result code
if (jsonlite::validate(query_results$content)){
  status_code = jsonlite::fromJSON(query_results$content, flatten = TRUE)$info$statuscode
} else {
  status_code = query_results$status
}
# Succesful status_code is 0
if (status_code == "0") status_code <- "200"
status_code <- as.character(status_code)

if (verbose == TRUE) message(paste0('HTTP Status Code: ', as.character(status_code)))


## Extract results -----------------------------------------------------------------------------------
# if there were problems with the results then return NA
if (status_code != "200") {
  if (!jsonlite::validate(query_results$content)) {
    # in cases like this, display the raw content but limit the length
    # in case it is really long.
    message(paste0('Error: ', strtrim(as.character(query_results$content), 100)))
  } else {
    content <- jsonlite::fromJSON(query_results$content, flatten = TRUE)
    if (!is.null(content$info$messages)) message(paste0('Error: ', content$info$messages))
    }
  return(NA_value)
}
# End error handling-----

# Note that flatten here is necessary in order to get rid of the
# nested dataframes that would cause dplyr::bind_rows (or rbind) to fail
content <- jsonlite::fromJSON(query_results$content, flatten = TRUE)

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
# MapQuest always return a default value (lat:39.4 long:-99.1) for non-found addresses
results <- dplyr::bind_rows(content$results$locations)

# rename lat/long columns
names(results)[names(results) == 'latLng.lat'] <- lat
names(results)[names(results) == 'latLng.lng'] <- long

# Prepare output----
if (full_results == FALSE) return(results[c(lat, long)])
else return(cbind(results[c(lat, long)], results[!names(results) %in% c(lat, long)]))


# Live test -----
library(tibble)
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  lat = "latt",
  long = "longgg",
  verbose = TRUE
)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  limit = 5,
  full_results = TRUE,
  return_addresses = FALSE,
  verbose = TRUE
)

ss <- tidygeocoder::geo(
  address = c("Plaza Mayor", "xxxxxxxxx", "George Street"),
  method = "mapquest",
  lat = "latitude",
  long = "longitude",
  full_results = TRUE,
  verbose = TRUE,
  custom_query = list(language = "de-DE")
)
glimpse(ss)

tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  limit = 1,
  mapquest_open = TRUE,
  full_results = TRUE,
  return_addresses = FALSE,
  verbose = TRUE
)

params <- tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  limit = 1,
  full_results = TRUE,
  return_addresses = FALSE,
  verbose = TRUE,
  custom_query = list(thumbMaps ="false", ignoreLatLngInput = TRUE)
)

glimpse(params)
# Silent
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  limit = 1,
  full_results = TRUE,
  return_addresses = TRUE,
  verbose = FALSE
)
# Try single result

tidygeocoder::geo(
  address = c("Plaza Mayor"),
  method = "mapquest",
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
  slice(1:101)

err <- address %>%
  geocode(
    address = "direcciones", method = "mapquest", full_results = TRUE,
    verbose = TRUE, lat = "latitude"
  )

err
# Error for api key
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  limit = 1,
  full_results = TRUE,
  return_addresses = TRUE,
  verbose = TRUE,
  custom_query = list(key="xxxx")
)

# Error on bad parameter
tidygeocoder::geo(
  address = c("Plaza Mayor", "George Street"),
  method = "mapquest",
  limit = 1,
  full_results = TRUE,
  return_addresses = TRUE,
  verbose = TRUE,
  custom_query = list(thumbMaps ="xxxx")
)

# Full batch test
address_ok <- tibble(direcciones = mapSpain::esp_munic.sf$name) %>%
  slice(1:100)

full_batch <- address_ok %>%
  geocode(
    address = "direcciones", method = "mapquest", full_results = TRUE,
    verbose = TRUE, lat = "latitude"
  )

full_batch
