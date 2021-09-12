

#' New function to replace method = 'cascade'
#'
#' @export
geocode_loop <- function(.tbl, queries, common_params = list(), lat = lat, long = long, stack = TRUE, cascade = FALSE) {
  # NSE eval ('lat' and lat are both accepted)
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # add common arguments to each query
  queries_prepped <- lapply(queries, function(x) {
    c(
      list(
        .tbl = .tbl,
        lat = lat,
        long = long
        ),
        common_params,
      x)})
  
  
  # iterate through the queries (list of lists) and execute each query
  # aggregate results in list object
  all_results <- list()
  for (query in queries_prepped) {
    
    result <- do.call(geocode, query)
    
    # which addresses were not found
    #na_indices <- is.na(result[[lat]]) | is.na(result[[long]])
    
    all_results <- c(all_results, list(result)) 
  }
  
  # stack all results in one dataframe if stack == TRUE
  # otherwise return list
  if (stack == FALSE) {
    return(all_results)
  } else {
    return(dplyr::bind_rows(all_results))
  }

}
