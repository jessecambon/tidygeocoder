#' Conveniance functions for calling the \code{\link{geo}} function
#' with a specified method.
#' 
#' @export
#' @param ... arguments to be passed to the \code{\link{geo}}
geo_census <- function(...) geo(method = 'census', ...)
  
#'@rdname geo_census
#' @export
geo_osm <- function(...) geo(method = 'osm', ...)

#'@rdname geo_census
#' @export
geo_geocodio <- function(...) geo(method = 'geocodio', ...)

#'@rdname geo_census
#' @export
geo_iq <- function(...) geo(method = 'iq', ...)

#' First try one method and then try another method 
#' 
#' @param ... arguments passed from and to the \code{\link{geo}} function
#' @param cascade_order a vector with two character method values showing 
#' the order to be attempted
#' @export
geo_cascade <- function(...) {
  # Note for both attempts we disable unique_only so as to keep the 
  # input address lists in line with the dataframe output so we can
  # more easily splice the results together
  input <- list(...)
  
  cascade_order <- input[['cascade_order']]
  
  print('cascade_order: ')
  print(cascade_order)
  
  print('input:')
  print(input)
  
  # first attempt 
  initial_results = do.call(geo, c(list(method = cascade_order[1], unique_only = FALSE),
                  input[!names(input) %in% c('method','unique_only', 'cascade_order')]))
  
  print('initial_results')
  print(initial_results)
  
  # find NA result indices for initial attempt
  na_indices <- is.na(initial_results[[input[['lat']]]]) | is.na(initial_results[[input[['long']]]])
  
  print('na_indices:')
  print(na_indices)
  
  # these are the addresses we retry
  retry_addresses <- lapply(input[names(input) %in% pkg.globals$address_arg_names], 
                function(x, bools) x[bools], na_indices)
  
  print('retry_addresses:')
  print(retry_addresses)
  
  if (length(retry_addresses) > 0) {
  
    retry_results = do.call(geo, c(retry_addresses,
          input[!names(input) %in% c(pkg.globals$address_arg_names, 'method', 'unique_only')],
            list(method = cascade_order[2], unique_only = FALSE)))
    
    print('retry_results: ')
    print(retry_results)
    
    ## combine census and retry results
    combi <- initial_results
    combi[na_indices,] <- retry_results
  }
  
  combi$geo_method <- ifelse(na_indices, cascade_order[2], cascade_order[1])
  
  # if user requested unique only then return distinct dataset
  if (input[['unique_only']] == TRUE) return(unique(combi)) 
  else return(combi)
  
}