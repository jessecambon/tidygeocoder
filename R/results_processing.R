### Functions for extracting data or error messages from geocoding results

#' Extract forward geocoding results
#'
#' @description
#' Parses the output of the [query_api] function for single
#' address geocoding (ie. not batch geocoding).
#' Latitude and longitude are extracted into the first two columns
#' of the returned dataframe.  Refer to  [query_api] for example
#' usage.
#'
#' @param method method name
#' @param response content from the geocoding service (returned by the [query_api] function)
#' @param full_results if TRUE then the full results (not just latitude and longitude)
#'   will be returned.
#' @param flatten if TRUE then flatten any nested dataframe content
#' @param limit only used for `r create_comma_list(pkg.globals$limit_passthru_methods, wrap = '"')` methods. Limits number of results per address.
#' @return geocoding results in tibble format
#' @seealso [get_api_query] [query_api] [geo]
#' @export
extract_results <- function(method, response, full_results = TRUE, flatten = TRUE, limit = 1) {
  # NOTE - the geo() function takes the output of this function and renames the
  # latitude and longitude columns

  NA_result <- get_na_value("lat", "long", 1)

  # extract latitude and longitude as a dataframe
  # latitude should be first column and longitude should be second column (column names don't matter here, just position)
  lat_lng <- switch(method,
    "census" = response$result$addressMatches$coordinates[c("y", "x")],
    "osm" = response[c("lat", "lon")],
    "iq" = response[c("lat", "lon")],
    "geocodio" = response$results$location[c("lat", "lng")],
    # note the application of the limit argument for google
    "google" = response$results$geometry$location[c("lat", "lng")],
    "opencage" = response$results$geometry[c("lat", "lng")],
    "mapbox" = data.frame(
      "lat" = response$features$center[[1]][2],
      "long" = response$features$center[[1]][1]
    ), # mapbox results are nested unnamed lists
    "here" = response$items$position[c("lat", "lng")],
    "tomtom" = response$results$position[c("lat", "lon")],
    "mapquest" = response$results$locations[[1]]$latLng[c("lat", "lng")],
    "bing" = extract_bing_latlng(response),
    "arcgis" = response$candidates$location[c("y", "x")],
    "geoapify" = data.frame(
      lat = response$features$geometry$coordinates[[1]][2],
      lon = response$features$geometry$coordinates[[1]][1]
    ) # geoapify returns GeoJSON
  )

  # Return NA if data is not empty or not valid (cannot be turned into a dataframe)
  if (is.null(names(lat_lng)) || all(sapply(lat_lng, is.null)) || length(lat_lng) == 0) {
    return(NA_result)
  }
  if (nrow(lat_lng) == 0 || ncol(lat_lng) != 2) {
    return(NA_result)
  }


  # For methods without a limit **API** parameter...
  # limit nrows in results to limit if limit is not NULL.
  if (method %in% pkg.globals$limit_passthru_methods) {
    rows_to_return <- min(limit, nrow(lat_lng))
    lat_lng <- lat_lng[1:rows_to_return, ]
  }

  # convert to numeric format. sapply is used because there could be multiple coordinates returned
  # for a single address
  lat_lng[, 1] <- sapply(lat_lng[, 1], function(x) as.numeric(as.character(x)), USE.NAMES = FALSE)
  lat_lng[, 2] <- sapply(lat_lng[, 2], function(x) as.numeric(as.character(x)), USE.NAMES = FALSE)

  if (full_results == TRUE) {
    # extract full results excluding latitude and longitude
    # note that lat/long are not excluded from the google results due to dataframe nesting
    results <- tibble::as_tibble(switch(method,
      "census" = response$result$addressMatches[!names(response$result$addressMatches) %in% c("coordinates")][1:rows_to_return, ],
      "osm" = response[!names(response) %in% c("lat", "lon")],
      "iq" = response[!names(response) %in% c("lat", "lon")],
      "geocodio" = response$results[!names(response$results) %in% c("location")],
      # note the application of the limit argument for google
      "google" = response$results[1:rows_to_return, ],
      "opencage" = response$results[!names(response$results) %in% c("geometry")],
      "mapbox" = response$features,
      "here" = response$items,
      "tomtom" = response$results,
      "mapquest" = response$results$locations[[1]],
      "bing" = response$resourceSets$resources[[1]],
      "arcgis" = response$candidates,
      "geoapify" =
        cbind(
          response$features$properties[!names(response$features$properties) %in% c("lat", "lon")],
          # bbox is not always returned. if it is null then return NA
          tibble::as_tibble(c(bbox = list(
            if (is.null(response$features$bbox)) list(NA_real_) else response$features$bbox
          )))
        )
    ))

    # Formatted address for mapquest
    if (method == "mapquest") {
      frmt_address <- format_address(
        results,
        c("street", paste0("adminArea", seq(6, 1)))
      )
      results <- tibble::as_tibble(cbind(frmt_address, results))
    }

    # add prefix to variable names that likely could be in our input dataset
    # to avoid variable name overlap
    for (var in c(pkg.globals$address_arg_names, "postcode")) {
      if (var %in% names(results)) {
        names(results)[names(results) == var] <- paste0(method, "_", var)
      }
    }

    combined_results <- dplyr::bind_cols(lat_lng, results)
  } else {
    combined_results <- lat_lng
  }

  if (flatten == TRUE) {
    return(jsonlite::flatten(combined_results))
  } else {
    return(combined_results)
  }
}

#' Extract reverse geocoding results
#'
#' @description
#' Parses the output of the [query_api] function for reverse geoocoding.
#' The address is extracted into the first column
#' of the returned dataframe. This function is not used for batch
#' geocoded results. Refer to [query_api] for example
#' usage.
#'
#' @param method method name
#' @param response  content from the geocoding service (returned by the [query_api] function)
#' @param full_results if TRUE then the full results (not just an address column)
#'   will be returned.
#' @param flatten if TRUE then flatten any nested dataframe content
#' @param limit only used for the `r create_comma_list(setdiff(pkg.globals$limit_passthru_methods, pkg.globals$no_reverse_methods), wrap = '"')`
#'    method(s). Limits number of results per coordinate.
#' @return geocoding results in tibble format
#' @seealso [get_api_query] [query_api] [reverse_geo]
#' @export
extract_reverse_results <- function(method, response, full_results = TRUE, flatten = TRUE, limit = 1) {
  # NOTE - the reverse_geo() function takes the output of this function and renames the
  # address column

  # For methods without a limit **API** parameter...
  # limit nrows in results to limit if limit is not NULL.
  if (method == "google") {
    rows_to_return <- min(nrow(response$results), limit)
  }

  NA_result <- tibble::tibble(address = as.character(NA))

  # extract the single line address
  address <- switch(method,
    "osm" = response["display_name"],
    "iq" = response["display_name"],
    "geocodio" = response$results["formatted_address"],
    # note the application of the limit argument for google
    "google" = response$results[1:rows_to_return, ]["formatted_address"],
    "opencage" = response$results["formatted"],
    "mapbox" = response$features["place_name"],
    "here" = response$items["title"],
    "tomtom" = response$addresses$address["freeformAddress"],
    "mapquest" = format_address(
      response$results$locations[[1]],
      c("street", paste0("adminArea", seq(6, 1)))
    ),
    "bing" = response$resourceSets$resources[[1]]["name"],
    "arcgis" = response$address["LongLabel"],
    "geoapify" = response$features$properties["formatted"]
  )

  # Return NA if data is empty or not valid (cannot be turned into a dataframe)
  if (is.null(names(address)) | all(sapply(address, is.null)) | length(address) == 0) {
    return(NA_result)
  }

  # convert to tibble
  address <- tibble::as_tibble(address)

  # check to make sure results aren't NA or the wrong width
  if (nrow(address) == 0 | ncol(address) != 1) {
    return(NA_result)
  }

  # extract other results (besides single line address)
  if (full_results == TRUE) {
    results <- tibble::as_tibble(switch(method,
      "osm" = extract_osm_reverse_full(response),
      "iq" = extract_osm_reverse_full(response),
      "geocodio" = response$results[!names(response$results) %in% c("formatted_address")],
      # note the application of the limit argument for google
      "google" = response$results[1:rows_to_return, ][!names(response$results) %in% c("formatted_address")],
      "opencage" = response$results[!names(response$results) %in% c("formatted")],
      "mapbox" = response$features[!names(response$features) %in% c("place_name")],
      "here" = response$items[!names(response$items) %in% c("title")],
      "tomtom" = response$addresses,
      "mapquest" = response$results$locations[[1]],
      "bing" = response$resourceSets$resources[[1]][names(response$resourceSets$resources[[1]]) != "name"],
      "arcgis" = response$address[names(response$address) != "LongLabel"],
      "geoapify" = response$features$properties[names(response$features$properties) != "formatted"]
    ))


    # add prefix to variable names that likely could be in our input dataset
    # to avoid variable name overlap
    for (var in c("lat", "lon", "long", "latitude", "longitude", "address")) {
      if (var %in% names(results)) {
        names(results)[names(results) == var] <- paste0(method, "_", var)
      }
    }
    combined_results <- dplyr::bind_cols(address, results)
  } else {
    combined_results <- address
  }

  if (flatten == TRUE) {
    return(jsonlite::flatten(combined_results))
  } else {
    return(combined_results)
  }
}

# Extracts errors from a raw response object and display them
# expected response is query_api(...)$content (ie. the raw content from the HTTP request)
# This function is called in reverse_geo() and geo()
extract_errors_from_results <- function(method, response, verbose) {
  # test if response contains JSON content
  if (!jsonlite::validate(response)) {
    # tomtom does not return JSON content on errors
    # in cases like this, display the raw content but limit the length
    # in case it is really long.
    message(paste0("Error: ", strtrim(as.character(response), 100)))
  } else {
    # parse JSON content
    raw_results <- jsonlite::fromJSON(response)

    # if results are blank
    if (length(raw_results) == 0) {
      if (verbose == TRUE) message("No results found")
    } else if ((method == "osm") && ("error" %in% names(raw_results))) {
      message(paste0("Error: ", raw_results$error$message))
    } else if ((method == "iq") && ("error" %in% names(raw_results))) {
      message(paste0("Error: ", raw_results$error))
    } else if ((method == "mapbox") && (!is.data.frame(raw_results$features))) {
      if ("message" %in% names(raw_results)) {
        message(paste0("Error: ", raw_results$message))
      }
    } else if ((method == "census") && ("errors" %in% names(raw_results))) {
      message(paste0("Error: ", raw_results$errors))
    } else if ((method == "opencage") && (!is.data.frame(raw_results$results))) {
      if (!is.null(raw_results$status$message)) {
        message(paste0("Error: ", raw_results$status$message))
      }
    } else if ((method == "geocodio") && (!is.data.frame(raw_results$results))) {
      if ("error" %in% names(raw_results)) {
        message(paste0("Error: ", raw_results$error))
      }
    } else if ((method == "google") && (!is.data.frame(raw_results$results))) {
      if ("error_message" %in% names(raw_results)) {
        message(paste0("Error: ", raw_results$error_message))
      }
    } else if ((method == "here") && (!is.data.frame(raw_results$items))) {
      if ("error_description" %in% names(raw_results)) {
        message(paste0("Error: ", raw_results$error_description))
      } else if ("title" %in% names(raw_results)) message(paste0("Error: ", raw_results$title))
    } else if ((method == "tomtom") && (!is.data.frame(raw_results$addresses)) && (!is.data.frame(raw_results$results))) {
      if ("errorText" %in% names(raw_results)) {
        message(paste0("Error: ", raw_results$errorText))
      } else if ("error" %in% names(raw_results)) {
        message(paste0("Error: ", raw_results$error))
      }
    } else if (method == "mapquest") {
      if (!is.null(raw_results$info$messages)) message(paste0("Error: ", raw_results$info$messages))
    } else if (method == "bing") {
      if ("errorDetails" %in% names(raw_results)) message(paste0("Error: ", raw_results$errorDetails, collapse = "\n"))
    } else if (method == "arcgis") {
      if ("error" %in% names(raw_results)) message(paste0("Error: ", raw_results$error$message, collapse = "\n"))
    } else if (method == "geoapify") message("Error: ", paste(raw_results$error, raw_results$message, sep = ", "))
  }
}
