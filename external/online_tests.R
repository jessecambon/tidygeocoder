## These tests query the geocoder services and require API keys to be installed
## so they are kept separate from the internal package tests (tests folder)

# get all method names
all_methods <- unique(tidygeocoder::api_parameter_reference[['method']])

### IMPORTANT: Select which methods you want to test #########################################

# Pick a value below for `methods_to_test` (the list of methods to test)

# Test all methods
methods_to_test <- all_methods 

# EXCLUDE methods with slow geocoder services
#methods_to_test <- setdiff(all_methods, tidygeocoder:::pkg.globals$single_first_methods)

#methods_to_test <- setdiff(all_methods, c('here', 'mapbox', 'bing'))
#methods_to_test <- c('census', 'osm', 'geocodio', 'opencage', 'google', 'geoapify', 'arcgis')
#methods_to_test <- c('census', 'osm', 'geocodio', 'opencage', 'arcgis')
methods_to_test <- "mapquest"


##############################################################################################


# exclude methods with no reverse geocoding capabilities
reverse_methods <- methods_to_test[!methods_to_test %in% tidygeocoder:::pkg.globals$no_reverse_methods]

library(tidygeocoder)
library(dplyr)
library(tibble)
library(testthat)

## Functions -----------------------------------------------------------------

# check forward geocoder results
check_forward_geocoding_results <- function(method, result, lat_name = 'lat', long_name = 'long', return_addresses = TRUE) {
  # label to include in error message so we know which method failed
  method_label = paste0('method = "', method, '"', ' ')
  
  # check that a tibble is returned
  expect_true(is_tibble(result), label = method_label)
  
  expected_col_names <- if (return_addresses == TRUE) c('address', lat_name, long_name) else c(lat_name, long_name)
  
  # check column names
  expect_equal(names(result)[1:length(expected_col_names)], expected_col_names, label = method_label)
  
  if (return_addresses == TRUE) {
    # check address column (input)
    expect_false(any(is.na(result[['address']])), label = method_label)
    expect_true(all(is.character(result[['address']])), label = method_label)
  }
  
  # check that all lat long values are numeric
  expect_true(all(is.numeric(result[[lat_name]])), label = method_label)
  expect_true(all(is.numeric(result[[long_name]])), label = method_label)
  
  # check that no results were NA
  expect_false(any(is.na(result[[lat_name]])), label = method_label)
  expect_false(any(is.na(result[[long_name]])), label = method_label)
}

check_reverse_geocoding_results <- function(method, result, address_name = 'address', lat_name = 'lat', long_name = 'long', return_coords = TRUE) {
  # label to include in error message so we know which method failed
  method_label = paste0('method = "', method, '"', ' ')
  
  # check that a tibble is returned
  expect_true(is_tibble(result), label = method_label)
  
  expected_col_names <- if (return_coords == TRUE) c('lat', 'long', address_name) else c(address_name)
  
  # check column names
  expect_equal(names(result)[1:length(expected_col_names)], expected_col_names, label = method_label)
  
  # check lat long
  if (return_coords == TRUE) {
    expect_true(all(is.numeric(result[[lat_name]])), label = method_label)
    expect_true(all(is.numeric(result[[long_name]])), label = method_label)
    expect_false(any(is.na(result[[lat_name]])), label = method_label)
    expect_false(any(is.na(result[[long_name]])), label = method_label)
  }
  
  # check address datatypes
  expect_true(all(is.character(result[[address_name]])), label = method_label)
  
  # check that addresses returned are not NA
  expect_false(any(is.na(result[[address_name]])), label = method_label)
}


## -----------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------


# test error catching for a single method
test_error_catching <- function(method, generic_args = list(), custom_args = list()) {
  
  # Error messages in a loop that iterates through methods
  # will not show the method unless you use a label
  # ie. we want to know which method failed
  method_label = paste0('method = "', method, '"', ' ')
  
  print(paste0('Invalid query handling: ', method))
  
  # We expect a warning because the query is invalid
  expect_warning(query_response <- query_api(tidygeocoder:::get_api_url(method), 
                                             query_parameters = tidygeocoder::get_api_query(method, generic_args, custom_args)),
                 label = method_label)
  
  # HTTP query should not be valid
  expect_true(200 != query_response$status, label = method_label)
  
  # Check to make sure an error message is extracted
  expect_message(tidygeocoder:::extract_errors_from_results(method, query_response$content, FALSE),
                 label = method_label)
}


# This test uses the census batch geocoder but it should apply to all methods
test_that("test forward batch limit", {
  
  louisville_addresses <- tidygeocoder::louisville %>% 
    head(10) %>%
    mutate(combi_addr = paste0(street, ' ', city, ', ', state, ' ', zip)) %>%
    pull(combi_addr)

  print(paste0('Testing batch limit'))
  expect_error(geo(louisville_addresses, method = 'census', full_results = TRUE, mode = 'batch',
            batch_limit = 5))
  
  expect_error(result1 <- geo(louisville_addresses, method = 'census', full_results = TRUE, 
                   batch_limit = 5))
  
  # check that a 1 row dataframe is returned
  # expect_true(is_tibble(result1))
  # expect_equal(nrow(result1), length(sample_addresses))
  
  # check address
  # expect_equal(result1$address, sample_addresses)
  
  # check that all lat long values are numeric
  # expect_true(all(is.numeric(result1$lat)))
  # expect_true(all(is.numeric(result1$long)))
})


test_that("test single forward geocoding", {
  
  sample_address <- '1600 Pennsylvania Ave NW Washington, DC'
  
  for (method in methods_to_test) {
    print(paste0('Forward single queries: ', method))
    
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    expect_true(is_tibble(result1 <- geo(sample_address, lat = lattt, long = longgg, 
                   method = method, full_results = TRUE, limit = 1)), label = method_label)
    
    check_forward_geocoding_results(method, result1, lat_name = 'lattt', long_name = 'longgg')
    
    # check number of rows
    expect_equal(nrow(result1), 1, label = method_label)
    
    # check address content
    expect_equal(result1$address, sample_address, label = method_label)
    
    # check geocoding a result that will not be found.
    # see https://github.com/jessecambon/tidygeocoder/issues/112
    # iq gives a warning
    expect_true(is_tibble(na_result1 <- geo('asdfghjkl', full_results = TRUE, limit = 1, method = method)),
                label = method_label)
  }
})

test_that("test forward batch geocoding", {
  # test the methods in 'methods_to_test' that have a batch function
  batch_methods_to_test <- intersect(methods_to_test, names(tidygeocoder:::batch_func_map))
  
  sample_addresses <- c(
    "1600 Pennsylvania Ave NW Washington, DC",
    "233 S Wacker Dr, Chicago, IL 60606"
  )
  
  test_addresses <- c('Paris', 'London') # for limit tests
  
  for (method in batch_methods_to_test) {
    print(paste0('Forward batch queries: ', method))
    
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    
    ## test limit = 1 batch geocoding  -----------------------------------------
    result1 <- geo(sample_addresses, method = method, lat = lattt, long = longgg, 
                   mode = 'batch', full_results = TRUE, limit = 1)
    
    check_forward_geocoding_results(method, result1, lat_name = 'lattt', long_name = 'longgg')
    
    # check number of rows 
    expect_equal(nrow(result1), length(sample_addresses), label = method_label)
    
    # check address content
    expect_equal(result1$address, sample_addresses, label = method_label)
    
    ## Test limit !=1 batch geocoding -------------------------------------------
    if (method != 'census') {
      expect_true(is_tibble(batch_null_limit_results <- geo(test_addresses, method = method, mode = 'batch', 
                                                            full_results = TRUE, limit = NULL, return_addresses = FALSE)),
                  label = method_label)
      check_forward_geocoding_results(method, batch_null_limit_results, return_addresses = FALSE)
      
      expect_true(is_tibble(batch_high_limit_results <- geo(test_addresses, method = method, mode = 'batch', 
                                                            full_results = TRUE, limit = 50, return_addresses = FALSE)),
                  label = method_label)
      check_forward_geocoding_results(method, batch_high_limit_results, return_addresses = FALSE)
    }
    
    # do a separate test for the census batch geocoder -----------------------------
    if (method == 'census') {
      census_test_addresses <- c('10 Main St NY, NY', '100 Main St, Louisville, KY')
      
      expect_true(is_tibble(census_batch_null_limit_results <- geo(census_test_addresses, method = 'census', 
                  return_addresses = FALSE, mode = 'batch', full_results = TRUE, limit = NULL)),
                  label = method_label)
      
      expect_true(is_tibble(census_batch_high_limit_results <- geo(census_test_addresses, method = 'census', 
                  return_addresses = FALSE, mode = 'batch', full_results = TRUE, limit = 5)),
                  label = method_label)
    }
  }
})

test_that("check single forward geocoding limit", {
  test_address <- "Paris"
  
  test_addresses <- c('Paris', 'London')
  
  for (method in methods_to_test[!methods_to_test %in% c('census')]) {
    print(paste0('forward geocoding limit: ', method))
    
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    ## Test single address geocoding
    expect_true(is_tibble(null_limit_results <- geo(test_address, method = method, 
                    mode = 'single', full_results = TRUE, limit = NULL)), 
                label = method_label)
    check_forward_geocoding_results(method, null_limit_results)
    
    expect_true(is_tibble(high_limit_results <- geo(test_address, method = method, 
                      mode = 'single', full_results = TRUE, limit = 50)),
                label = method_label)
    check_forward_geocoding_results(method, high_limit_results)
    
    if (method == 'census') {
      census_test_address <- '10 Main St NY, NY'

      expect_true(is_tibble(census_null_limit_results <- geo(census_test_address, method = 'census', 
                                                             mode = 'single', full_results = TRUE, limit = NULL)),
                  label = method_label)
      
      expect_true(is_tibble(census_high_limit_results <- geo(census_test_address, method = 'census', 
                                                             mode = 'single', full_results = TRUE, limit = 50)),
                  label = method_label)
    }
  }
})

test_that("test reverse single geocoding", {
  sample_lat <- 38.8792 
  sample_long <- -76.9818
  sample_lats <- c(38.89770, 41.87886)
  sample_longs <- c(-77.03655, -87.63592)
  
  for (method in reverse_methods) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    print(paste0('Reverse single queries: ', method))
    expect_true(is_tibble(result <- reverse_geo(lat = sample_lat, long = sample_long, address = addr,
                          method = method, full_results = TRUE)),
                label = method_label)
    
    check_reverse_geocoding_results(method, result, address_name = 'addr')
    
    
    # Limit check
    expect_true(is_tibble(limit_null <- reverse_geo(lat = sample_lats, long = sample_longs, mode = 'single',
                method = method, full_results = TRUE, limit = NULL, return_coords = FALSE)),
                label = method_label)
    
    check_reverse_geocoding_results(method, limit_null, return_coords = FALSE)
    
    # expect this to fail with Mapbox: https://github.com/jessecambon/tidygeocoder/issues/104
    if (method != 'mapbox') {
      expect_true(is_tibble(high_limit <- reverse_geo(lat = sample_lats, long = sample_longs, mode = 'single',
                  method = method, full_results = TRUE, limit = 35, return_coords = FALSE)),
                  label = method_label)
      
      check_reverse_geocoding_results(method, high_limit, return_coords = FALSE)
    }
  }
})

test_that("test reverse batch geocoding", {
  # white house (Washington, DC), Willis tower (chicago)
  sample_lats <- c(38.89770, 41.87886)
  sample_longs <- c(-77.03655, -87.63592)
  
  if (length(sample_lats) != length(sample_longs)) stop('invalid coordinates')
  
  # test the methods in 'methods_to_test' that have a batch function
  batch_methods_to_test <- intersect(methods_to_test, names(tidygeocoder:::reverse_batch_func_map))
  
  for (method in batch_methods_to_test) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    print(paste0('Reverse batch queries: ', method))
    result <- reverse_geo(lat = sample_lats, long = sample_longs, mode = 'batch',
                    address = addr, method = method, full_results = TRUE)
    
    check_reverse_geocoding_results(method, result, address_name = 'addr')
    
    # check number of rows
    expect_equal(nrow(result), length(sample_lats), label = method_label)
    
    # Limit check
    expect_true(is_tibble(limit_null <- reverse_geo(lat = sample_lats, long = sample_longs, mode = 'batch',
                method = method, full_results = TRUE, limit = NULL, return_coords = FALSE)),
                label = method_label)
    
    check_reverse_geocoding_results(method, limit_null, return_coords = FALSE)
    
    expect_true(is_tibble(high_limit <- reverse_geo(lat = sample_lats, long = sample_longs, mode = 'batch',
                method = method, full_results = TRUE, limit = 35, return_coords = FALSE)),
                label = method_label)
    
    check_reverse_geocoding_results(method, high_limit, return_coords = FALSE)
    
  }
})

# Pass an invalid query to each service and make sure
# error messages are caught
test_that("test error catching", {
    test_error_catching('osm', list(format = 'invalid'))
    test_error_catching('census', list())
    test_error_catching('arcgis', list(format = 'invalid'))
    
    # for methods requiring an api key, passing an invalid key
    # should cause an error
    for (method in tidygeocoder::api_key_reference[['method']]) {
      if (method != "bing"){
        test_error_catching(method, list(api_key = 'invalid')) 
      } else {
        test_error_catching(method, custom_args = list(key = 'invalid', q = "invalid")) 
      }
    }
})