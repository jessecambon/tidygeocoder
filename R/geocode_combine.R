
#' Combine multiple geocoding queries
#' 
#' @description Passes address inputs in character vector form to the
#'  [geocode_combine] function for geocoding.
#' 
#' @param queries list of lists parameter. Each list contains parameters for a query
#'   (ie. `list(list(method = 'osm'), list(method = 'census'), ...)`)
#' @param global_params list parameter. Contains parameters to be used for all queries. 
#'   (ie. `list(full_results = TRUE, unique_only = TRUE)`)
#' @inheritParams geo
#' @param ... arguments passed to the [geocode_combine] function
#' @inherit geo return
#' @examples
#' \donttest{
#' geo_combine(queries = list(list(method = 'census'), list(method = 'osm')), 
#'   address = c("100 Main St New York, NY", "Paris", "Not a Real Address"))
#' }
#' @seealso [geocode_combine] [geo] [geocode]
#' @export
geo_combine <- function(queries, global_params = list(), address = NULL, 
                     street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL, 
                     lat = lat, long = long, ...) {
  
  lat <- rm_quote(deparse(substitute(lat)))
  long <- rm_quote(deparse(substitute(long)))
  
  # prepare data for geocode_combine() function
   input_df <- tibble::tibble(
    address = address, 
    street = street, city = city, county = county, state = state, postalcode = postalcode, country = country
  )
  
   # Combine user input global parameters with the address parameters that are needed
   global_params_combined <- global_params
   for (colname in colnames(input_df)) {
     global_params_combined[[colname]] <- colname
   }
   
  return(
    geocode_combine(.tbl = input_df, queries = queries, global_params = global_params_combined, lat = lat, long = long, ...)
  )
}


#' Combine multiple geocoding queries
#' 
#' @description Executes multiple geocoding queries on a dataframe input and combines
#'  the results. To use a character vector input instead, see the [geo_combine] function.
#' 
#' @param queries list of lists parameter. Each list contains parameters for a query
#'   (ie. `list(list(method = 'osm'), list(method = 'census'), ...)`)
#' @param global_params list parameter. Contains parameters to be used for all queries.
#'   (ie. `list(full_results = TRUE, unique_only = TRUE)`)
#' @param return_list if TRUE then results will be returned in a named list. If FALSE (default) 
#'   then all results will be combined into a single dataframe.
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
geocode_combine <- function(.tbl, queries, global_params = list(), query_names = NULL, return_list = FALSE, cascade = TRUE, lat = lat, long = long) {
  
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
  
  # print('all_param_lists:')
  # print(all_param_lists)
  
  # remove NULL values from parameter name lists
  # all_param_names_no_null <- sapply(all_param_names, function(x) {
  #   x[sapply(x, is.null)] <- NULL
  #   return(x)
  #   }, USE.NAMES = FALSE)
  
  # which address arguments were used in the queries
  #used_address_args <- unique(intersect(all_param_names_no_null, pkg.globals$address_arg_names))
  
  # which columns are used to store addresses
  used_address_colnames <- unique(unlist(sapply(all_param_lists, 
            function(x) unlist(x[names(x) %in% pkg.globals$address_arg_names], use.names = FALSE))))
  
  #message(paste0('used_address_colnames: ', used_address_colnames))

  # TODO: make a workaround so return_input can be FALSE if limit = 1 for all queries (ie.
  # we can track which address is which by index) ??
  
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
      stop('query_names parameter must contain one name per query provided. see ?geocode_combine')
    }
    
    if (any(duplicated(query_names)) == TRUE) {
      stop('query_names values should be unique. See ?geocode_combine')
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
    # create query name column before combining results into single dataframe
    all_results_labeled <- lapply(
      names(all_results), function(x) 
        dplyr::bind_cols(all_results[[x]], tibble::tibble(query = x))
      )
    
    # put all results into a single dataframe
    bound_data <- dplyr::bind_rows(all_results_labeled)
    
    # remove .id column if it is present
    bound_data <- bound_data[!names(bound_data) %in% '.id']
    
    # reorder the dataset to match the order it was received in before returning it
    proper_order <- unique(.tbl[used_address_colnames])
    proper_order[['.id']] <- 1:nrow(proper_order)
    
    # join to get our id column so we can sort
    bound_data_joined <- dplyr::left_join(bound_data, proper_order, by = used_address_colnames)
      
    # sort the dataset
    bound_data_joined <- bound_data_joined[order(bound_data_joined[['.id']]), ]
    
    # remove .id column before returning the dataset
    return(bound_data_joined[!names(bound_data_joined) %in% '.id'])
  }
}