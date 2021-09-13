
#' New function to replace method = 'cascade'
#' 
#' @description 
#' 
#' @inheritParams geo
#' @param ... arguments passed to the [geocode_loop] function
#' @inherit geo return
#' @export
geo_loop <- function(address = NULL, 
                     street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL, ...) {
  
  # Check argument inputs
  check_address_argument_datatype(address, 'address')
  check_address_argument_datatype(street, 'street')
  check_address_argument_datatype(city, 'city')
  check_address_argument_datatype(county, 'county')
  check_address_argument_datatype(state, 'state')
  check_address_argument_datatype(postalcode, 'postalcode')
  check_address_argument_datatype(country, 'country')
  
  tibble::tibble(
    address = address, 
    street = street, city = city, county = county, state = state, postalcode = postalcode, country = country
  )
  
  # TODO: call geocode_loop() while specifying address columns for non-NULL parameters
  
}


#' New function to replace method = 'cascade'
#' 
#' @description 
#' 
#' @param queries list of lists parameter. Each list contains parameters for a query
#'   (ie. `list(list(method = 'osm'), list(method = 'census'), ...)`)
#' @param common_params list parameter. Contains arguments that should be used for all queries.
#'   (ie. `list(full_results = TRUE, unique_only = TRUE)`)
#' @param stack if TRUE then a single dataframe will be returned. When stack = FALSE
#'   then a list of dataframes, one per each query, will be returned.
#' @param cascade if TRUE then only addresses that are not found will be attempted by
#'   the following query. If FALSE then all queries will attempt to geocode all addresses.
#' @inheritParams geocode
#' @inherit geo return
#' @export
geocode_loop <- function(.tbl, queries, common_params = list(), stack = TRUE, cascade = TRUE, lat = lat, long = long) {
  
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
