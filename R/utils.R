## Put common utilities here

#' Get raw results back from an API
#' @param api_url Base URL of the API. query parameters are appended to this
#' @param query_parameters parameters for query in list form
#' @param content_encoding Encoding to be used for parsing content. 
#' @return raw results from the query
#' Census uses "ISO-8859-1", all other services use "UTF-8"
#' @export
query_api <- function(api_url, query_parameters, content_encoding='UTF-8') {

  response <- httr::GET(url = api_url, query = query_parameters)
  return(jsonlite::fromJSON(httr::content(response, as = 'text', encoding = content_encoding))
  )
}

# How many seconds have elapsed since start time t0 (as defined by a t0 <- Sys.time() call) 
get_seconds_elapsed <- function(t0) {
  return(as.numeric(difftime(Sys.time(), t0, units = 'secs')))
}

pause_until <- function(start_time,min_time,debug=FALSE) {
  ## Make sure the proper amount of time has elapsed for the query per min_time
  seconds_elapsed <- get_seconds_elapsed(start_time)
  if (debug == TRUE) message(paste0('Time elapsed: ', round(seconds_elapsed,1),' seconds'))
  
  # Sleep if necessary to make query take the minimum amount of time
  if (seconds_elapsed < min_time) {
    Sys.sleep(min_time - seconds_elapsed)
    if (debug == TRUE) message(paste0('Time elapsed (after sleep): ', 
                                      round(get_seconds_elapsed(start_time),1),' seconds'))
  }
}
