

#' New function to replace method = 'cascade'
#'
#' @export
geocode_loop <- function(.tbl, queries, common_params = list(), stack = TRUE, cascade = TRUE, lat = 'lat', long = 'long') {
  
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # TODO: throw error if cascade == TRUE and return_addresses == FALSE (check all list args passed)
  # since we need addresses to know what was or wasn't found
  
  # TODO: add preemptive checking of parameters by running all the queries with no_query = TRUE and a tryCatch
  
  # TODO: add a column for stacked results that shows which data is from which query
  
  # add common arguments to each query
  queries_prepped <- lapply(queries, function(x) {
    c(
      list(.tbl = .tbl),
        common_params,
      x)})
  
  
  # iterate through the queries (list of lists) and execute each query
  # aggregate results in list object
  all_results <- list()
  not_found <- tibble::tibble()
  for (query in queries_prepped) {
    
    if (cascade == TRUE) {
      # adjust the input dataframe based on which addresses were not found in prior query
      if (nrow(not_found) != 0) {
        query[['.tbl']] <- not_found
      }
    }
    
    result <- do.call(geocode, query)
    
    # which addresses were not found
    if (cascade == TRUE) {
      #distinct_found <- dplyr::distinct(result[names(result) %in% intersect(names(query), pkg.globals$address_arg_names]))
      not_found <- result[is.na(result[[lat]]) | is.na(result[[long]]), intersect(colnames(result), colnames(.tbl))]
    }
    
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

# geocode_loop(sample_addresses, list(list(method = 'census'), list(method = 'osm')), list(address = 'addr'), cascade = TRUE)
