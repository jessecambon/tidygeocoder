
#' Combine multiple geocoding queries
#' 
#' @description Passes address inputs to the [geocode_combine] function
#'   for geocoding.
#' 
#' @param queries list of lists parameter. Each list contains parameters for a query
#'   (ie. `list(list(method = 'osm'), list(method = 'census'), ...)`)
#' @param global_params list parameter. Contains arguments that should be used for all queries.
#'   (ie. `list(full_results = TRUE, unique_only = TRUE)`)
#' @inheritParams geo
#' @param ... arguments passed to the [geocode_combine] function
#' @inherit geo return
#' @examples
#' \donttest{
#' geo_combine(queries = list(list(method = 'census')), address = c(""))
#' }
#' @seealso [geocode_combine] [geo] [geocode]
#' @export
geo_combine <- function(queries, global_params = list(), address = NULL, 
                     street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL, ...) {
  
  # Check argument inputs
  check_argument_inputs(address, street, city, county, state, postalcode, country, 'geo_combine')
  
  # prepare data for geocode_combine() function
   input_df <- tibble::tibble(
    address = address, 
    street = street, city = city, county = county, state = state, postalcode = postalcode, country = country
  )
  
   # Combine user input global parameters with the addres parameters that are needed
   global_params_combined <- global_params
   for (colname in colnames(input_df)) {
     global_params_combined[[colname]] <- colname
   }
   
  return(
    geocode_combine(.tbl = input_df, queries = queries, global_params = global_params_combined, ...)
  )
}


#' Combine multiple geocoding queries
#' 
#' @description Executes multiple geocoding queries on a dataframe input and combines
#'  the results.
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
#' @examples
#' \donttest{
#' geocode_combine(sample_addresses,
#'  queries = list(list(method = 'census'), list(method = 'osm')),
#'  global_params = list(address = 'addr'), cascade = TRUE)
#' }
#' @seealso [geo_combine] [geo] [geocode]
#' @export
geocode_combine <- function(.tbl, queries, global_params = list(), query_names = NULL, stack = TRUE, cascade = TRUE, lat = lat, long = long) {
  
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # TODO: make a workaround so return_input can be FALSE if limit = 1 for all queries (ie.
  # we can track which address is which by index)
  if (!is.null(global_params[['return_input']]) && global_params[["return_input"]] == FALSE) stop('return_input must be set to TRUE', .call = FALSE)
  for (query in queries) {
    if (!is.null(query[["return_input"]]) && query[["return_input"]] == FALSE) stop('return_input must be set to TRUE', .call = FALSE)
  }
  
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
      stop('query_names parameter must contain one name per query provided. see ?geocode_combine')
    }
  }
  
  # Sanity check all queries (ie. make sure no queries have a mistyped argument, etc.)
  for (query in queries_prepped) {
    query[['no_query']] <- TRUE
    
    tryCatch(
      expr = {
        suppressMessages(do.call(geocode, query))
      },
      error = function(e) {
        message('The following error was produced:\n')
        message(e)
        
        message('\n\nBy these query parameters:\n')
        for (name in names(query)) {
          # don't display .tbl parameter for now
          if (name != '.tbl') message(paste0(name, ' = ', query[[name]]))
        } 
        
      },
      finally = {
        message('')
      }
    )    
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
    
    na_indices <- is.na(result[[lat]]) | is.na(result[[long]])
    
    # which addresses were not found
    if (cascade == TRUE) {
      #distinct_found <- dplyr::distinct(result[names(result) %in% intersect(names(query), pkg.globals$address_arg_names]))
      not_found <- result[na_indices, intersect(colnames(result), colnames(.tbl))]
    }
    
    # addresses that were found (non-NA results)
    found <- result[!is.na(result[[lat]]) & !is.na(result[[long]]), ]
    
    # aggregate results
    all_results <- c(all_results, list(found)) 
  }
  
  
  names(all_results) <- query_names
  if (nrow(not_found) != 0) all_results[["none"]] <- not_found
  
  # stack all results in one dataframe if stack == TRUE
  # otherwise return list
  if (stack == FALSE) {
    return(all_results)
  } else {
    
    # add query name column before stacking
    all_results_labeled <- lapply(
      names(all_results), function(x) 
        dplyr::bind_cols(all_results[[x]], tibble::tibble(query = x)))
    
    return(dplyr::bind_rows(all_results_labeled))
  }

}