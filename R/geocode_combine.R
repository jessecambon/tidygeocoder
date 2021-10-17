
get_queries_parameter_documentation <- function() {
  return(c(
      "a list of queries, each provided as a list of parameters. The queries are",
      "executed in the order provided by the [geocode] function.",
      "(ex. `list(list(method = 'osm'), list(method = 'census'), ...)`)"
    ))
}

get_global_params_parameter_documentation <- function() {
  return(c(
    "a list of parameters to be used for all queries",
    "(ex. `list(address = 'address', full_results = TRUE)`)"
  ))
}


#' Combine multiple geocoding queries
#' 
#' @description Passes address inputs in character vector form to the
#'  [geocode_combine] function for geocoding.
#'  
#'  Note that address inputs must be specified for queries either in `queries` (for each query)
#'  or `global_params` (for all queries). For example `global_params = list(address = 'address')` 
#'  passes addresses provided in the `address` parameter to all queries.
#' 
#' @param queries `r get_queries_parameter_documentation()`
#' @param global_params `r get_global_params_parameter_documentation()`
#' @inheritParams geo
#' @param ... arguments passed to the [geocode_combine] function
#' @inherit geo return
#' @examples
#' \donttest{
#' 
#' options(tidygeocoder.progress_bar = FALSE)
#' example_addresses <- c("100 Main St New York, NY", "Paris", "Not a Real Address")
#' 
#' geo_combine(
#'     queries = list(
#'         list(method = 'census'),
#'         list(method = 'osm')
#'     ),
#'     address = example_addresses,
#'     global_params = list(address = 'address')
#'   )
#'   
#' geo_combine(
#'   queries = list(
#'       list(method = 'arcgis'), 
#'       list(method = 'census', mode = 'single'),
#'       list(method = 'census', mode = 'batch')
#'   ),
#'   global_params = list(address = 'address'),
#'   address = example_addresses,
#'   cascade = FALSE,
#'   return_list = TRUE
#' )
#' 
#' geo_combine(
#'    queries = list(
#'       list(method = 'arcgis', address = 'city'),
#'       list(method = 'osm', city = 'city', country = 'country')
#'    ),
#'    city = c('Tokyo', 'New York'),
#'    country = c('Japan', 'United States'),
#'    cascade = FALSE
#' )
#' }
#' @seealso [geocode_combine] [geo] [geocode]
#' @export
geo_combine <- function(queries, global_params = list(), address = NULL, 
                     street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, 
                     country = NULL, lat = lat, long = long, ...) {
  
  # NSE - converts lat and long parameters to character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # Check address arguments -------------------------------------
  address_pack <- package_addresses(address, street, city, county, 
                                    state, postalcode, country)
  
  # prepare data for geocode_combine() function -----------------
   input_df <- tibble::tibble(
    address = address,
    street = street, city = city, county = county, state = state, postalcode = postalcode, country = country
  )
   
   # pass arguments to geocode_combine() as a list. lat and long arguments did not work when passed directly
   arguments_to_pass <- c(list(
     .tbl = input_df, queries = queries, global_params = global_params, lat = lat, long = long), 
     list(...)
     )

   return(
     do.call(geocode_combine, arguments_to_pass)
   )
}


#' Combine multiple geocoding queries
#' 
#' @description Executes multiple geocoding queries on a dataframe input and combines
#'  the results. To use a character vector input instead, see the [geo_combine] function.
#'  Queries are executed by the [geocode] function. See example usage 
#'  in `vignette("tidygeocoder")`.
#'  
#'  Query results are by default labelled to show which query produced each result. Labels are either
#'  placed in a `query` column (if `return_list = FALSE`) or used as the names of the returned list 
#'  (if `return_list = TRUE`). By default the `method` parameter value of each query is used as a query label.
#'  If the same `method` is used in multiple queries then a number is added according 
#'  to the order of the queries (ie. `osm1`, `osm2`, ...). To provide your own custom query labels
#'  use the `query_names` parameter.
#' 
#' @param queries `r get_queries_parameter_documentation()`
#' @param global_params `r get_global_params_parameter_documentation()`
#' @param return_list if TRUE then results from each service will be returned as separate 
#'   dataframes. If FALSE (default) then all results will be combined into a single dataframe.
#' @param cascade if TRUE (default) then only addresses that are not found by a geocoding 
#'   service will be attempted by subsequent queries. If FALSE then all queries will 
#'   attempt to geocode all addresses.
#' @param query_names optional vector with one label for each query provided 
#'   (ex. `c('geocodio batch', 'geocodio single')`).
#' @inheritParams geocode
#' @inherit geo return
#' @examples
#' \donttest{
#' 
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' sample_addresses %>%
#'   geocode_combine(
#'     queries = list(list(method = 'census'), list(method = 'osm')),
#'     global_params = list(address = 'addr'), cascade = TRUE)
#' 
#' more_addresses <- tibble::tribble(
#'      ~street_address, ~city, ~state,        ~zip_cd,
#'      "624 W DAVIS ST #1D",   "BURLINGTON", "NC", 27215,
#'      "201 E CENTER ST #268", "MEBANE",     "NC", 27302,
#'      "100 Wall Street",      "New York",   "NY", 10005,
#'      "Bucharest",            NA,           NA,   NA
#'      )
#'  
#'  more_addresses %>%        
#'    geocode_combine( 
#'      queries = list(
#'          list(method = 'census', mode = 'batch'),
#'          list(method = 'census', mode = 'single'),
#'          list(method = 'osm')
#'       ),
#'      global_params = list(street = 'street_address', 
#'        city = 'city', state = 'state', postalcode = 'zip_cd'),
#'      query_names = c('census batch', 'census single', 'osm')
#'    )
#'    
#'  more_addresses %>%
#'    geocode_combine( 
#'      queries = list(
#'          list(method = 'census', mode = 'batch', street = 'street_address', 
#'        city = 'city', state = 'state', postalcode = 'zip_cd'),
#'          list(method = 'arcgis', address = 'street_address')
#'       ),
#'      cascade = FALSE,
#'      return_list = TRUE
#'    )
#' }
#' @seealso [geo_combine] [geo] [geocode]
#' @export
geocode_combine <- function(.tbl, queries, global_params = list(),  
                          return_list = FALSE, cascade = TRUE, query_names = NULL,
                          lat = lat, long = long) {
  
  # NSE - converts lat and long parameters to character values
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  stopifnot(
    is.data.frame(.tbl), 
    is.list(queries), 
    # either queries is an empty list or contains other lists
    length(queries) == 0 || all(sapply(queries, is.list)),
    is.list(global_params), is.logical(return_list), is.logical(cascade),
    is.null(query_names) || is.character(query_names)
  )
  
  # combine all parameter lists into a list of lists
  all_param_lists <- queries
  all_param_lists[[length(queries) + 1]] <- global_params 
  
  # get all parameter names
  all_param_names <- unlist(sapply(all_param_lists, names))
  
  # which columns are used to store addresses
  used_address_colnames <- unique(unlist(sapply(all_param_lists, 
            function(x) unlist(x[names(x) %in% pkg.globals$address_arg_names], use.names = FALSE))))
  
  if (cascade == TRUE) {
    for (query in all_param_lists) {
      if (!is.null(query[["return_input"]]) && query[["return_input"]] == FALSE) {
        stop('return_input must be set to TRUE for geocode_combine()', call. = FALSE)
      }
    }
  }
  
  # add global arguments to each query
  queries_prepped <- lapply(queries, function(x) {
    c(
      list(.tbl = .tbl, lat = lat, long = long),
        global_params,
      x)})
  
  # Set default query names and check user input query names
  if (is.null(query_names)) {
    # default query names to the method arguments and fill in 'osm' if the method argument isn't provided
    query_names <- unlist(lapply(queries_prepped, function(q) if (!is.null(q[['method']])) q[['method']] else 'osm'))
    
    # number duplicate query names if necessary (to prevent duplicates)
    # ie. 'osm1', 'osm2', etc.
    for (name in unique(query_names)) {
      # if the given name occurs more than once in query_names then iterate through and add numbers
      if ((sum(query_names == name)) > 1) {
        i <- 1
        dup_num <- 1
        for (i in 1:length(query_names)) {
          if (query_names[[i]] == name) {
            query_names[[i]] <- paste0(query_names[[i]], as.character(dup_num), collapse = '')
            dup_num <- dup_num + 1
          }
        }
      }
    }
    
  } else {
    if (length(query_names) != length(queries)) {
      stop('query_names parameter must contain one name per query provided. See ?geocode_combine')
    }
    
    if (any(duplicated(query_names)) == TRUE) {
      stop('query_names values should be unique. See ?geocode_combine')
    }
    
    if (any(trimws(query_names) == '')) {
      stop('query_names values should not be blank. See ?geocode_combine')
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
    })}
  
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
      } else if (i != 1) {
        break # break loop - all addresses are geocoded
      }
    }
    
    # use geocode() to execute the query
    result <- do.call(geocode, query)
    na_indices <- is.na(result[[lat]]) | is.na(result[[long]])
    
    # which addresses were not found
    not_found <- result[na_indices, intersect(colnames(result), colnames(.tbl))]
    found <- result[!is.na(result[[lat]]) & !is.na(result[[long]]), ]
    
    # aggregate results. if using cascade then separate the not-found addresses
    all_results <- if (cascade == TRUE) c(all_results, list(found)) else c(all_results, list(result))
  }
  
  names(all_results) <- query_names[1:length(all_results)]
  
  # if there are addresses that no method found then in cascade then 
  # separate them into their own list item
  if (cascade == TRUE) {
    if(nrow(not_found) != 0) all_results[[length(all_results) + 1]] <- not_found
  }
  
  # bind all results into one dataframe if return_list == FALSE
  # otherwise return list
  if (return_list == TRUE) {
    return(all_results)
  } else {
    
    # label the dataframes contained in the all_results list with a 'query' column
    all_results_labeled <- mapply(function(x, y) dplyr::bind_cols(x, tibble::tibble(query = y)), 
                       all_results, names(all_results), SIMPLIFY = FALSE)
    
    # put all results into a single dataframe
    bound_data <- dplyr::bind_rows(all_results_labeled)
    
    # remove .id column if it is present
    bound_data <- bound_data[!names(bound_data) %in% '.id']
    
    # reorder the dataset to match the order it was received in before returning it
    proper_order <- unique(.tbl[used_address_colnames])
    proper_order[['.id']] <- 1:nrow(proper_order)
    
    # join to get our .id column so we can sort the output dataset
    bound_data_joined <- dplyr::left_join(bound_data, proper_order, by = used_address_colnames)
      
    # sort the dataset
    bound_data_joined <- bound_data_joined[order(bound_data_joined[['.id']]), ]
    
    # remove .id column before returning the dataset
    return(bound_data_joined[!names(bound_data_joined) %in% '.id'])
  }
}