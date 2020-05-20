## Put common utilities here


# Get raw results back from an API

#' @param api_url Base URL of the API. query parameters are appended to this
#' @param query_parameters parameters for query in list form
#' @param content_encoding Encoding to be used for parsing content. 
#' @return raw results from the query
#' Census uses "ISO-8859-1", all other services use "UTF-8"
#' 
get_raw_results <- function(api_url, query_parameters, content_encoding='UTF-8') {
  
  response <- httr::GET(url = api_url, query = query_parameters)
  
  return(
    jsonlite::fromJSON(httr::content(response, as = 'text', encoding = content_encoding))
  )
}