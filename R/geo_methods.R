#' Convenience functions for calling the \code{\link{geo}} function
#' with a specified method.
#' 
#' @export
#' @param ... arguments to be passed to the \code{\link{geo}} function
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


#' Convenience function for calling the \code{\link{geo}} function
#' with method = 'cascade'
#' 
#' @param ... arguments passed from and to the \code{\link{geo}} function
#' @param cascade_order a vector with two character values for the method argument 
#' in the order in which the geocoder services will be attempted
#' @export
geo_cascade <- function(..., cascade_order = c('census', 'osm')) {
  # Note for both attempts we disable unique_only so as to keep the 
  # input address lists in line with the dataframe output so we can
  # more easily splice the results together
  input <- list(...)
  
  if ('lat' %in% names(input)) lat <- input[['lat']]
  else lat <- 'lat'
  if ('long' %in% names(input)) long <- input[['long']]
  else long <- 'long'
  
  # if the input is an unnamed list then pass it directly, else remove certain named elements
  # this is necessary because names(input) returns NULL if input is an unnamed list
  if (is.null(names(input))) {
    initial_parameters <- input
    unique_results_requested <- FALSE
  }
  else {
    initial_parameters <- input[!names(input) %in% c('method', 'unique_only', 'cascade_order')]
    unique_results_requested <- ifelse('unique_only' %in% names(input), input[['unique_only']], FALSE)
  }
  initial_parameters <- c(initial_parameters, list(method = cascade_order[1], unique_only = FALSE))
  
  # first attempt 
  initial_results = do.call(geo, initial_parameters)
  
  # find NA result indices for initial attempt
  na_indices <- is.na(initial_results[[lat]]) | is.na(initial_results[[long]])
  
  if (any(na_indices)) {
    # if some results weren't found then we retry
    if (is.null(names(initial_parameters))) retry_addresses <- input[na_indices]
    else retry_addresses <- lapply(input[names(input) %in% pkg.globals$address_arg_names], 
                  function(x, bools) x[bools], na_indices)

    retry_results = do.call(geo, c(retry_addresses,
          input[!names(input) %in% c(pkg.globals$address_arg_names, 'method', 'unique_only')],
            list(method = cascade_order[2], unique_only = FALSE)))
    
    ## combine census and retry results
    combi <- initial_results
    combi[na_indices,] <- retry_results
    combi$geo_method <- ifelse(na_indices, cascade_order[2], cascade_order[1])
  } else {
    # if all addresses were found then just return initial results
    combi <- initial_results
    combi$geo_method <- cascade_order[1]
  }

  # if user requested unique only then return distinct dataset
  if (unique_results_requested == TRUE) return(unique(combi)) 
  else return(combi)
}