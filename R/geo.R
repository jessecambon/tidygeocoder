
## IMPORTANT: All new batch geocoding functions must be added to batch_func_map
# the geo() function references this list to find batch geocoding functions (batch_geocoding.R)
# maps method names to batch functions
batch_func_map <- list(
  geocodio = batch_geocodio,
  census = batch_census,
  here = batch_here,
  tomtom = batch_tomtom,
  mapquest = batch_mapquest,
  bing = batch_bing
)

# call geo function with progress bar
progress_geo <- function(pb = NULL, ...) {
  results <- geo(...)
  if (!is.null(pb)) pb$tick()
  return(results)
}

#' Geocode addresses
#'
#' @description
#' Geocodes addresses given as character values. The [geocode]
#' function utilizes this function on addresses contained in dataframes.
#' See example usage in `vignette("tidygeocoder")`.
#'
#' Note that not all geocoding services support certain address component
#' parameters. For example, the Census geocoder only covers the United States
#' and does not have a "country" parameter.
#'
#' Refer to [api_parameter_reference],
#' [min_time_reference], and [batch_limit_reference] for more details on
#' geocoding service parameters and usage.
#'
#' This function uses the [get_api_query], [query_api], and
#' [extract_results] functions to create, execute, and parse geocoder
#' API queries.
#'
#' @param address single line address (ie. '1600 Pennsylvania Ave NW, Washington, DC').
#'    Do not combine with the address component arguments below
#'    (`street`, `city`, `county`, `state`, `postalcode`, `country`).
#' @param street street address (ie. '1600 Pennsylvania Ave NW')
#' @param city city (ie. 'Tokyo')
#' @param county county (ie. 'Jefferson')
#' @param state state (ie. 'Kentucky')
#' @param postalcode postalcode (ie. zip code if in the United States)
#' @param country country (ie. 'Japan')
#'
#' @param method `r get_method_documentation(reverse = FALSE)`
#' @param lat latitude column name. Can be quoted or unquoted (ie. `lat` or `"lat"`).
#' @param long longitude column name. Can be quoted or unquoted (ie. `long` or `"long"`).
#' @param limit `r get_limit_documentation(reverse = FALSE, df_input = FALSE)`
#'
#' @param full_results `r get_full_results_documentation(reverse = FALSE)`
#' @param mode `r get_mode_documentation(reverse = FALSE)`
#' @param unique_only only return results for unique inputs if TRUE
#' @param return_addresses return input addresses with results if TRUE. Note that
#'    most services return the input addresses with `full_results = TRUE` and setting
#'    return_addresses to FALSE does not prevent this.
#'
#' @param min_time minimum amount of time for a query to take (in seconds). If NULL
#' then min_time will be set to the default value specified in [min_time_reference].
#' @param progress_bar if TRUE then a progress bar will be displayed
#'   for single input geocoding (1 input per query). By default the progress bar
#'   will not be shown for code executed when knitting R Markdown files or code within
#'   an RStudio notebook chunk. Can be set permanently with `options(tidygeocoder.progress_bar = FALSE)`.
#' @param quiet if TRUE then console messages that are displayed by default
#'   regarding queries will be suppressed. FALSE is default.
#'   Can be set permanently with `options(tidygeocoder.quiet = TRUE)`.
#' @param api_url custom API URL. If specified, the default API URL will be overridden.
#'  This parameter can be used to specify a local Nominatim server, for instance.
#' @param timeout query timeout (in minutes)
#'
#' @param flatten if TRUE (default) then any nested dataframes in results are flattened if possible.
#'    Note that in some cases results are flattened regardless such as for Geocodio batch geocoding.
#' @param batch_limit  `r get_batch_limit_documentation(reverse = FALSE)`
#' @param verbose if TRUE then detailed logs are output to the console. FALSE is default. Can be set
#'    permanently with `options(tidygeocoder.verbose = TRUE)`
#' @param no_query if TRUE then no queries are sent to the geocoding service and verbose is set to TRUE.
#'    Used for testing.

#' @param custom_query API-specific parameters to be used, passed as a named list
#'  (ex. `list(extratags = 1)`.
#'
#' @param api_options a named list of parameters specific to individual services.
#'   (ex. `list(geocodio_v = 1.6, geocodio_hipaa = TRUE)`). Each parameter begins
#'   with the name of the `method` (service) it applies to. The possible parameters
#'   are shown below with their default values.
#'
#'   - `census_return_type` (default: `r pkg.globals$default_api_options$census_return_type`): set to `"geographies"` to return
#'     additional geography columns. Make sure to use `full_results = TRUE` if using
#'     the "geographies" setting.
#'   - `iq_region` (default: "`r pkg.globals$default_api_options$iq_region`"): set to "eu" to use the European Union API endpoint
#'   - `geocodio_v` (default: `r pkg.globals$default_api_options$geocodio_v`): the version number of the Geocodio API to be used
#'   - `geocodio_hipaa` (default: `r pkg.globals$default_api_options$geocodio_hipaa`): set to `TRUE` to use the HIPAA compliant
#'      Geocodio API endpoint
#'   - `mapbox_permanent` (default: `r pkg.globals$default_api_options$mapbox_permanent`): set to `TRUE` to use the `mapbox.places-permanent`
#'      endpoint. Note that this option should be used only if you have applied for a permanent
#'      account. Unsuccessful requests made by an account that does not have access to the
#'      endpoint may be billable.
#'   - `mapquest_open` (default: `r pkg.globals$default_api_options$mapquest_open`): set to `TRUE` to use the Open Geocoding endpoint which
#'      relies solely on OpenStreetMap data
#'   - `here_request_id`: this parameter would return a previous HERE batch job,
#'      identified by its RequestID. The RequestID of a batch job is displayed
#'      when `verbose` is TRUE. Note that this option would ignore the
#'      current `address` parameter on the request, so the `return_addresses` or `return_coords`
#'      parameters need to be FALSE.
#'
#' @return tibble (dataframe)
#' @examples
#' \donttest{
#' options(tidygeocoder.progress_bar = FALSE)
#'
#' geo(
#'   street = "600 Peachtree Street NE", city = "Atlanta",
#'   state = "Georgia", method = "census"
#' )
#'
#' geo(
#'   address = c("Tokyo, Japan", "Lima, Peru", "Nairobi, Kenya"),
#'   method = "osm"
#' )
#'
#' geo("100 Main St New York, NY",
#'   full_results = TRUE,
#'   method = "census", api_options = list(census_return_type = "geographies")
#' )
#'
#' geo(
#'   county = "Jefferson", state = "Kentucky", country = "US",
#'   method = "osm"
#' )
#' }
#' @seealso [geocode] [api_parameter_reference] [min_time_reference] [batch_limit_reference]
#' @export
geo <-
  function(address = NULL,
           street = NULL,
           city = NULL,
           county = NULL,
           state = NULL,
           postalcode = NULL,
           country = NULL,
           method = "osm",
           lat = "lat",
           long = "long",
           limit = 1,
           full_results = FALSE,
           mode = "",
           unique_only = FALSE,
           return_addresses = TRUE,
           min_time = NULL,
           progress_bar = show_progress_bar(),
           quiet = getOption("tidygeocoder.quiet", FALSE),
           api_url = NULL,
           timeout = 20,
           flatten = TRUE,
           batch_limit = NULL,
           verbose = getOption("tidygeocoder.verbose", FALSE),
           no_query = FALSE,
           custom_query = list(),
           api_options = list()
           ) {

    # NSE - Quote unquoted vars without double quoting quoted vars
    # end result - all of these variables become character values
    lat <- rm_quote(deparse(substitute(lat)))
    long <- rm_quote(deparse(substitute(long)))

    # set the api_optons[["init"]] parameter if it is NULL
    api_options <- initialize_init(api_options)
    
    # apply api options defaults for options not specified by the user
    api_options <- apply_api_options_defaults(method, api_options)

    # capture all function arguments including default values as a named list.
    # IMPORTANT: make sure to put this statement before any other variables are defined in the function
    all_args <- as.list(environment())
    all_args$api_options[["init"]] <- FALSE # follow up queries are not the initial query

    # Check parameter arguments --------------------------------------------------------

    check_api_options(method, api_options, FALSE, return_addresses)

    # Check argument inputs
    if (is.null(address) && is.null(street) && is.null(city) && is.null(county) && is.null(state) && is.null(postalcode) && is.null(country)) {
      stop("No address data provided", call. = FALSE)
    }

    check_address_argument_datatype(address, "address")
    check_address_argument_datatype(street, "street")
    check_address_argument_datatype(city, "city")
    check_address_argument_datatype(county, "county")
    check_address_argument_datatype(state, "state")
    check_address_argument_datatype(postalcode, "postalcode")
    check_address_argument_datatype(country, "country")

    if (!is.null(api_options[["census_return_type"]])) {
      if (!(api_options[["census_return_type"]] %in% c("geographies", "locations"))) {
        stop("Invalid census_return_type argument. See ?geo", call. = FALSE)
      }
    }

    stopifnot(
      is.logical(verbose), is.logical(no_query), is.logical(flatten),
      is.logical(full_results), is.logical(unique_only), is.logical(return_addresses),
      is.logical(progress_bar), is.logical(quiet),
      is.numeric(timeout), timeout >= 0,
      is.list(custom_query)
    )

    check_verbose_quiet(verbose, quiet, reverse = FALSE)
    check_common_args("geo", mode, limit, batch_limit, min_time)
    check_method(method, reverse = FALSE, mode, batch_func_map)

    if (no_query == TRUE) verbose <- TRUE
    start_time <- Sys.time() # start timer

    ## Address parsing and deduplication -------------------------------------------------------
    address_pack <- package_addresses(
      address, street, city, county,
      state, postalcode, country
    )

    # count number of unique addresses
    num_unique_addresses <- nrow(address_pack$unique) # unique addresses
    NA_value <- get_na_value(lat, long, rows = num_unique_addresses) # filler result to return if needed

    if (verbose == TRUE) message(paste0("\nNumber of Unique Addresses: ", num_unique_addresses))

    # If no valid/non-blank addresses are passed then return NA
    if (num_unique_addresses == 1 && all(is.na(address_pack$unique))) {
      if (verbose == TRUE) message(paste0("No non-blank non-NA addreses found. Returning NA results."))
      return(unpackage_inputs(address_pack, NA_value, unique_only, return_addresses))
    }

    ### Parameter Check ------------------------------------------------------
    # check if limit and address parameters that are used are valid for the method
    legal_parameters <- get_generic_parameters(method)
    
    # add illegal parameters that are selected to this vector
    illegal_params <- c()

    # If a parameter is used that is not supported by the method then throw an error
    for (param in colnames(address_pack$unique)) {
      if (!(param %in% legal_parameters)) {
        illegal_params <- c(illegal_params, param)
      }
    }

    if (length(illegal_params) > 0) {
      param_message <- paste0(
        'The following parameter(s) are not supported for the "',
        method, '"',
        " method:\n\n", paste0(illegal_params, collapse = " "),
        "\n\nSee ?api_parameter_reference for more details."
      )
      stop(param_message, call. = FALSE)
    }

    # Single Address geocoding -------------------------------------------------------------
    # If there are multiple addresses and we are using a method without a batch geocoder function
    # OR the user has explicitly specified single address geocoding then call the
    # single address geocoder in a loop (ie. recursively call this function)

    # Exception for geocoding services that should default to single instead of batch
    if (method %in% pkg.globals$single_first_methods && mode != "batch") {
      mode <- "single"
    }

    # construct arguments for a single address query
    # note that non-address related fields go to the MoreArgs argument of mapply
    # since we aren't iterating through them
    single_addr_args <- c(
      list(FUN = progress_geo),
      as.list(address_pack$unique),
      list(
        MoreArgs = all_args[!names(all_args) %in% pkg.globals$address_arg_names],
        USE.NAMES = FALSE, SIMPLIFY = FALSE
      )
    )

    # Single address geocoding is used if the method has no batch function or if
    # mode = 'single' was specified
    if ((api_options[["init"]] == TRUE) && (mode != "batch") && (!(method %in% names(batch_func_map)) || ((num_unique_addresses == 1) || (mode == "single")))) {
      if (quiet == FALSE) {
        query_start_message(method, num_unique_addresses, reverse = FALSE, batch = FALSE)
      }

      if (progress_bar == TRUE) {
        # intialize progress bar
        pb <- create_progress_bar(
          num_unique_addresses
        )
        # add progress bar object to query
        single_addr_args$MoreArgs$pb <- pb
      }

      # Geocode each address individually by recalling this function with mapply
      list_coords <- do.call(mapply, single_addr_args)

      # tell user how long the query took if the progress bar hasn't already
      if (quiet == FALSE && progress_bar == FALSE) {
        query_complete_message(start_time)
      }

      # rbind the list of tibble dataframes together
      stacked_results <- dplyr::bind_rows(list_coords)

      # note that return_inputs has been set to FALSE here since addresses will already
      # be returned in the first geo function call (if asked for)
      return(unpackage_inputs(address_pack, stacked_results, unique_only, FALSE))
    }

    # Batch geocoding --------------------------------------------------------------------------
    if (api_options[["init"]] == TRUE) {
      if (verbose == TRUE) message("Executing batch geocoding...\n")

      # check for conflict between limit and return_addresses arguments
      check_limit_for_batch(limit, return_addresses, reverse = FALSE)

      # set batch limit to default if not specified
      if (is.null(batch_limit)) batch_limit <- get_batch_limit(method)
      if (verbose == TRUE) {
        message(paste0(
          "Batch limit: ",
          format(batch_limit, big.mark = ",")
        ))
      }

      # Enforce batch limit if needed
      if (num_unique_addresses > batch_limit) {
        batch_limit_exceeded <- TRUE
        batch_limit_exceeded_message <- paste0(
          format(num_unique_addresses, big.mark = ","),
          " unique addresses found which exceeds the batch limit of ",
          format(batch_limit, big.mark = ","), "."
        )

        # batch limit error
          stop(batch_limit_exceeded_message, call. = FALSE)

          # warning(paste0(
          #   batch_limit_exceeded_message, "\n",
          #   "Only geocoding the first ", format(batch_limit, big.mark = ","), " unique addresses. ",
          #   "All other addresses will have NA results."
          # ))

          # apply batch limit to query
          # batch_unique_addresses <- address_pack$unique[1:batch_limit, ]
      } else {
        # if batch limit wasn't exceeded we just will use the unique addresses in address_pack
        batch_limit_exceeded <- FALSE
        batch_unique_addresses <- address_pack$unique
      }

      if (method == "here") check_here_return_input(api_options[["here_request_id"]], return_addresses, reverse = FALSE)


      # Display message to user on the batch query
      if (quiet == FALSE) {
        query_start_message(
          method,
          min(batch_limit, num_unique_addresses),
          reverse = FALSE,
          batch = TRUE,
          display_time = FALSE
        )
        flatten_override_warning(flatten, method, reverse = FALSE, batch = TRUE)
      }

      # return NA results if no_query == TRUE
      if (no_query == TRUE) {
        return(unpackage_inputs(address_pack, NA_value, unique_only, return_addresses))
      }

      # call the appropriate function for batch geocoding according the the batch_func_map named list
      # if batch limit was exceeded then apply that limit
      batch_results <- do.call(batch_func_map[[method]], c(
        list(batch_unique_addresses),
        all_args[!names(all_args) %in% pkg.globals$address_arg_names]
      ))

      # If batch limit was exceeded we need to add some NA results to make our results
      # line up with the input addresses
      if (batch_limit_exceeded == TRUE) {
        batch_filler <- get_na_value(lat, long, rows = num_unique_addresses - batch_limit)
        batch_results <- dplyr::bind_rows(batch_results, batch_filler)
      }

      # if quiet = FALSE, tell user how long batch query took
      if (quiet == FALSE) {
        query_complete_message(start_time)
      }

      # map the raw results back to the original addresses that were passed if there are duplicates
      return(unpackage_inputs(address_pack, batch_results, unique_only, return_addresses))
    }

    #################################################################
    #### Code past this point is for geocoding a single address #####
    #################################################################

    # Start to build 'generic' query as named list -------------------------
    generic_query <- list()

    # construct query with single-line address or address components
    if (!is.null(address)) {
      search <- "onelineaddress" # for census API URL
      generic_query[["address"]] <- address_pack$unique$address
    } else {
      search <- "address" # for census API URL
      if (!is.null(street)) generic_query[["street"]] <- address_pack$unique$street
      if (!is.null(city)) generic_query[["city"]] <- address_pack$unique$city
      if (!is.null(county)) generic_query[["county"]] <- address_pack$unique$county
      if (!is.null(state)) generic_query[["state"]] <- address_pack$unique$state
      if (!is.null(postalcode)) generic_query[["postalcode"]] <- address_pack$unique$postalcode
      if (!is.null(country)) generic_query[["country"]] <- address_pack$unique$country
    }

    # Set API URL (if not already set) ---------------------------
    if (is.null(api_url)) {
      api_url <- get_api_url(method,
        reverse = FALSE, return_type = api_options[["census_return_type"]],
        search = search, geocodio_v = api_options[["geocodio_v"]], iq_region = api_options[["iq_region"]],
        mapbox_permanent = api_options[["mapbox_permanent"]], mapquest_open = api_options[["mapquest_open"]],
        geocodio_hipaa = api_options[["geocodio_hipaa"]]
      )
    }

    ## Workaround for Mapbox/TomTom - The search_text should be in the API URL
    api_url <- api_url_modification(method, api_url, generic_query, custom_query, reverse = FALSE)

    # Set min_time if not set based on usage limit of service
    if (is.null(min_time)) min_time <- get_min_query_time(method)

    # add limit and api_key to generic query
    generic_query <- add_common_generic_parameters(generic_query, method, no_query, limit)

    # Convert our generic query parameters into parameters specific to our API (method)
    api_query_parameters <- get_api_query(method, generic_query, custom_query)

    # Execute Single Address Query -----------------------------------------
    if (verbose == TRUE) display_query(api_url, api_query_parameters)

    # return NA results if no_query = TRUE
    if (no_query == TRUE) {
      return(unpackage_inputs(address_pack, NA_value, unique_only, return_addresses))
    }
    query_results <- query_api(api_url, api_query_parameters, method = method)

    if (verbose == TRUE) message(paste0("HTTP Status Code: ", as.character(query_results$status)))

    ## Extract results -----------------------------------------------------------------------------------
    # if there were problems with the results then return NA
    if (query_results$status != 200) {
      extract_errors_from_results(method, query_results$content, verbose)
      results <- NA_value
    } else {
      # Extract results. Use the full_results and flatten parameters
      # to control the output
      results <- extract_results(method, jsonlite::fromJSON(query_results$content), full_results, flatten, limit)

      # Name the latitude and longitude columns in accordance with lat/long arguments
      names(results)[1] <- lat
      names(results)[2] <- long
    }

    # Make sure the proper amount of time has elapsed for the query per min_time
    pause_until(start_time, min_time, debug = verbose)
    if (verbose == TRUE) message("") # insert ending line break if verbose

    return(unpackage_inputs(address_pack, results, unique_only, return_addresses))
  }
