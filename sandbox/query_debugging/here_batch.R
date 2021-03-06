api_url <- NULL
timeout <- 20
limit <- 1
addresses <- c("Calle Mayor", "Springfield")
lat <- "latttt"
long <- "longgg"
verbose <- TRUE



if (is.null(api_url)) {
  api_url <- "https://batch.geocoder.ls.hereapi.com/6.2/jobs"
}

NA_batch <-
  get_na_value(lat, long, rows = nrow(addresses_tbl)) # filler result to return if needed


# https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/quick-start-batch-geocode.html

# Select outcols
# Output structure differs from single geocoding
# https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/read-batch-request-output.html
outcols <- c(
  "displayLatitude",
  "displayLongitude",
  "locationLabel",
  "street",
  "district",
  "city",
  "postalCode",
  "county",
  "state",
  "country",
  "relevance",
  "mapViewBottomRightLatitude",
  "mapViewBottomRightLongitude",
  "mapViewTopLeftLatitude",
  "mapViewTopLeftLongitude"
)

# Create custom query with specific params ----
# https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/request-parameters.html
custom_query <- list(
  maxresults = limit,
  indelim = "|",
  outdelim = "|",
  # Required
  action = "run",
  # Required
  outputcombined = TRUE,
  # Required,
  outcols = paste0(outcols, collapse = ",")
)

query_parameters <- get_api_query("here",
  list(limit = limit, api_key = get_key("here")),
  custom_parameters = custom_query
)

# Create body of the request
# Needs to have recID and searchText
addresses_tbl <- tibble::tibble(
  recId = seq_len(length(addresses)),
  searchText = addresses
)

# Plain text, | dlm, \n new line
body <- paste(
  "recID|searchText\n",
  paste(addresses_tbl$recId, "|", addresses_tbl$searchText, collapse = "\n")
)


# HERE Batch Geocoder is a 3 step process:
# 1. Send the request and get a job id
# 2. Wait - Status of the job can be checked
# 3. Results

# Step 1: Run job and retrieve id ----
job <- httr::POST(
  api_url,
  query = query_parameters,
  body = body,
  encode = "raw",
  httr::timeout(60 * timeout)
)
# Timer (optional)
init_process <- Sys.time()
job_result <- httr::content(job)

# Message here if unsuccessful
if (is.null(job_result$Response$MetaInfo$RequestId)) {
  message(paste0("Error: ", job_result$Details))
  return(NA_batch)
}

# Retrieve request_id
request_id <- job_result$Response$MetaInfo$RequestId
request_id <- "E2bc948zBsMCG4QclFKCq3tddWYCsE9g"
if (verbose) {
  message("HERE RequestID: ", request_id)
}

# Step 2: Check job until is done ----
# https://developer.here.com/documentation/batch-geocoder/dev_guide/topics/job-status.html
current_status <- ""
if (verbose) {
  message("HERE: Batch job accepted. Processing...")
}

while (!current_status %in% c("cancelled", "failed", "completed")) {
  Sys.sleep(3) # Arbitrary, 3sec
  status <- httr::GET(
    url = paste0(api_url, "/", request_id),
    query = list(
      action = "status",
      apiKey = get_key("here")
    )
  )
  status_get <- httr::content(status)
  prev_status <- current_status
  current_status <- as.character(status_get$Response$Status)
  if (prev_status != current_status && verbose) {
    message("Status: ", current_status)
    message(
      "Processed: ",
      status_get$Response$ProcessedCount,
      ",",
      "Pending: ",
      status_get$Response$PendingCount,
      ",", "Errors: ",
      status_get$Response$ErrorCount
    )
  }
}
status_get
# Message here
update_time_elapsed <- get_seconds_elapsed(init_process)
if (verbose) {
  print_time("HERE: Batch job processed in", update_time_elapsed)
}

# Delete non-complete jobs

if (current_status != "completed") {
  delete <- httr::DELETE(
    url = paste0(api_url, "/", request_id),
    query = list(apiKey = get_key("here"))
  )

  if (verbose) {
    message("HERE: Batch job failure")
  }
  return(NA_batch)
}



# Step 3: Download and parse ----
batch_results <- httr::GET(
  url = paste0(api_url, "/", request_id, "/result"),
  query = list(
    apiKey = get_key("here"),
    outputcompressed = FALSE
  )
)
result_content <- httr::content(batch_results)

# Parse results----
result_parsed <- tibble::as_tibble(read.table(
  text = result_content,
  header = TRUE, sep = "|"
))

# Merge to original addresses and output
results <-
  tibble::as_tibble(merge(addresses_tbl, result_parsed, by = "recId", all.x = TRUE))
results <- results[, !names(results) %in% "recId"]
names(results)[names(results) == "displayLatitude"] <- lat
names(results)[names(results) == "displayLongitude"] <- long


results

# param management----
limit <- 1
outcols <- c(
  "displayLatitude", "displayLongitude", "locationLabel", "street",
  "district", "city", "postalCode", "county", "state", "country",
  "relevance", "mapViewBottomRightLatitude",
  "mapViewBottomRightLongitude", "mapViewTopLeftLatitude",
  "mapViewTopLeftLongitude"
)
custom_here_query <- list(
  maxresults = limit,
  indelim = "|",
  outdelim = "|", # Required
  outputcombined = TRUE, # Required,
  outcols = paste0(outcols, collapse = ",")
)
custom_query <- list(outcols = "abcde")

custom_here_query <- custom_here_query[!names(custom_here_query) %in% names(custom_query)]

custom_query <- c(custom_query, custom_here_query)
custom_query[["outdelim"]]


custom_query

# Live tests----

test1 <- tidygeocoder::geo(
  address = c("Calle Mayor", "Springfield", "xx"),
  method = "here",
  mode = "batch",
  verbose = TRUE,
  limit = 1
)


test1

# retrieve previous JOB a43TItKAmS75XUdN3co24Ok9DxUinkF4
# It may fail if another apiKey is in use


tidygeocoder::geo(
  address = c("Calle Mayor", "Springfield", "xx"),
  method = "here",
  verbose = TRUE,
  mode = "batch",
  return_addresses = FALSE,
  full_results = TRUE,
  here_request_id = "a43TItKAmS75XUdN3co24Ok9DxUinkF4",
  limit = 1
)

# Batch non existing
tidygeocoder::geo(
  address = c("Calle Mayor", "Springfield", "xx"),
  method = "here",
  verbose = TRUE,
  return_addresses = FALSE,
  mode = "batch",
  full_results = TRUE,
  here_request_id = "abcdefghei1",
  limit = 1
)

test2 <- tidygeocoder::geo(
  address = c("Calle Mayor", "Springfield", "------"),
  method = "here",
  verbose = TRUE,
  full_results = TRUE,
  mode = "batch",
  return_addresses = FALSE,
  limit = 3
)
test2

# Error due to delim
tidygeocoder::geo(
  address = c("------"),
  method = "here",
  verbose = TRUE,
  full_results = TRUE,
  mode = "batch",
  limit = 1,
  custom_query = list(indelim = "tt")
)

test3 <- tidygeocoder::geo(
  address = c("------", "Barcelona"),
  method = "here",
  verbose = TRUE,
  full_results = TRUE,
  mode = "batch",
  limit = 1,
  custom_query = list(
    indelim = ";",
    outdelim = ":"
  )
)

test3

test4 <- tidygeocoder::geo(
  address = c("Calle Mayor, Madrid, Spain"),
  method = "here",
  verbose = FALSE,
  full_results = TRUE,
  mode = "batch",
  limit = 1,
  custom_query = list(
    outcols = "locationLabel,locationLabel"
  )
)
test4


test5 <- tidygeocoder::geo(
  address = c("Gran Via, Madrid, Spain"),
  method = "here",
  verbose = FALSE,
  full_results = TRUE,
  mode = "batch",
  limit = 1,
  custom_query = list(
    outcols = "locationLabel,displayLatitude,displayLongitude"
  )
)
test5
