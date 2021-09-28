
#' New function to replace method = 'cascade'
#' 
#' @description  placeholder
#' 
#' @param queries list of lists parameter. Each list contains parameters for a query
#'   (ie. `list(list(method = 'osm'), list(method = 'census'), ...)`)
#' @param global_params list parameter. Contains arguments that should be used for all queries.
#'   (ie. `list(full_results = TRUE, unique_only = TRUE)`)
#' @inheritParams geo
#' @param ... arguments passed to the [geocode_loop] function
#' @inherit geo return
#' @export
geo_loop <- function(queries, global_params = list(), address = NULL, 
                     street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL, ...) {
  
  # Check argument inputs
  check_argument_inputs(address, street, city, county, state, postalcode, country, 'geo_loop')
  
  # prepare data for geocode_loop() function
   input_df <- tibble::tibble(
    address = address, 
    street = street, city = city, county = county, state = state, postalcode = postalcode, country = country
  )
  
   global_params_combi <- c(
     global_params, 
     list(address = address, street = street, city = city, county = county,
           state = state, postalcode = postalcode, country = country)
     )
   
  return(
    geocode_loop(.tbl = input_df, global_params = global_params_combi, ...)
  )
}


#' New function to replace method = 'cascade'
#' 
#' @description 
#' 
#' @param queries list of lists parameter. Each list contains parameters for a query
#'   (ie. `list(list(method = 'osm'), list(method = 'census'), ...)`)
#' @param global_params list parameter. Contains arguments that should be used for all queries.
#'   (ie. `list(full_results = TRUE, unique_only = TRUE)`)
#' @param stack if TRUE then a single dataframe will be returned. When stack = FALSE
#'   then a list of dataframes, one per each query, will be returned.
#' @param cascade if TRUE then only addresses that are not found will be attempted by
#'   the following query. If FALSE then all queries will attempt to geocode all addresses.
#' @param query_names an optional vector of names to use for labeling the queries.
#' @inheritParams geocode
#' @inherit geo return
#' @export
geocode_loop <- function(.tbl, queries, global_params = list(), query_names = NULL, stack = TRUE, cascade = TRUE, lat = lat, long = long) {
  
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # TODO: throw error if cascade == TRUE and return_addresses == FALSE (check all list args passed)
  # since we need addresses to know what was or wasn't 
  
  # TODO: add a 'not found' column/item in the list that is returned to separate out
  # the addresses that are not found
  
  # TODO: add preemptive checking of parameters by running all the queries with no_query = TRUE and a tryCatch
  
  # TODO: order the results properly
  
  # add global arguments to each query
  queries_prepped <- lapply(queries, function(x) {
    c(
      list(.tbl = .tbl),
        global_params,
      x)})
  
  # Set default query names and check user input query names
  if (is.null(query_names)) {
    query_names <- unlist(lapply(queries_prepped, function(q) q[['method']]))
  } else {
    if (length(query_names) != length(queries)) {
      stop('query_names parameter must contain one name per query provided. see ?geocode_loop')
    }
  }
  
  # iterate through the queries (list of lists) and execute each query
  # aggregate results in list object
  all_results <- list()
  not_found <- tibble::tibble()
  for (i in 1:length(queries_prepped)) {
    
    query <- queries_prepped[[i]]
    
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
    
    # only return non-NA results unless we are at the last query in the loop
    result_to_return <- if (i == length(queries_prepped)) result else {
      result[!is.na(result[[lat]]) & !is.na(result[[long]]), ]
    }
    
    # aggregate results
    all_results <- c(all_results, list(result_to_return)) 
  }
  
  names(all_results) <- query_names
  
  # stack all results in one dataframe if stack == TRUE
  # otherwise return list
  if (stack == FALSE) {
    return(all_results)
  } else {
    
    # add query name column before stacking
    all_results_labeled <- lapply(
      query_names, function(x) 
        dplyr::bind_cols(all_results[[x]], tibble::tibble(query = x)))
    
    return(dplyr::bind_rows(all_results_labeled))
  }

}

# a <- geocode_loop(sample_addresses, list(list(method = 'census'), list(method = 'osm')), list(address = 'addr'), cascade = TRUE)
