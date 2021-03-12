## These tests query the geocoder services and require API keys to be installed
## so they are kept separate from the internal package tests (tests folder)

## IMPORTANT: edit 'methods_to_test' to only include methods you have API keys for

# get all method names
all_methods <- unique(tidygeocoder::api_parameter_reference[['method']])

### Select which methods you want to test #################
methods_to_test <- all_methods
#methods_to_test <- c('census', 'osm', 'opencage')
###########################################################

library(tidygeocoder)
library(dplyr)


test_that("test forward batch limit", {
  
  sample_addresses <- tidygeocoder::louisville %>% head(10) %>%
    mutate(combi_addr = paste(street, city,',', state, zip)) %>%
    pull(combi_addr)

  print(paste0('Testing batch limit'))
  expect_error(geo(sample_addresses, method = 'census', full_results = TRUE, 
            batch_limit = 5, batch_limit_error = TRUE))
  
  expect_warning(result1 <- geo(sample_addresses, method = 'census', full_results = TRUE, 
                   batch_limit = 5, batch_limit_error = FALSE))
  
  # check that a 1 row dataframe is returned
  expect_true(is.data.frame(result1))
  expect_equal(nrow(result1), length(sample_addresses))
  
  # check address
  expect_equal(result1$address, sample_addresses)
  
  # check that all lat long values are numeric
  expect_true(all(is.numeric(result1$lat)))
  expect_true(all(is.numeric(result1$long)))
})


test_that("test single forward geocoding", {
  
  sample_address <- '1600 Pennsylvania Ave NW Washington, DC'
  
  for (method in methods_to_test) {
    print(paste0('Forward single queries: ', method))
    result1 <- geo(sample_address, method = method, full_results = TRUE)
    
    # check that a 1 row dataframe is returned
    expect_true(is.data.frame(result1))
    expect_equal(nrow(result1), 1)
    
    # check address
    expect_equal(result1$address, sample_address)
    
    # check lat long datatype
    expect_true(is.numeric(result1$lat))
    expect_true(is.numeric(result1$long))
    
    # check that results were not NA
    expect_false(is.na(result1$lat))
    expect_false(is.na(result1$long))
  }
})

test_that("test forward batch geocoding", {
  # test the methods in 'methods_to_test' that have a batch function
  batch_methods_to_test <- intersect(methods_to_test, names(tidygeocoder:::batch_func_map))
  
  sample_addresses <- c(
    "1600 Pennsylvania Ave NW Washington, DC",
    "233 S Wacker Dr, Chicago, IL 60606"
  )
  
  for (method in batch_methods_to_test) {
    print(paste0('Forward batch queries: ', method))
    result1 <- geo(sample_addresses, method = method, full_results = TRUE)
    
    # check that a 1 row dataframe is returned
    expect_true(is.data.frame(result1))
    expect_equal(nrow(result1), length(sample_addresses))
    
    # check address
    expect_equal(result1$address, sample_addresses)
    
    # check that all lat long values are numeric
    expect_true(all(is.numeric(result1$lat)))
    expect_true(all(is.numeric(result1$long)))
    
    # check that no results were NA
    expect_false(any(is.na(result1$lat)))
    expect_false(any(is.na(result1$long)))
  }
})

test_that("test reverse single geocoding", {
  sample_lat <- 38.8792 
  sample_long <- -76.9818
  
  # Currently census is the only method that doesn't support reverse geocoding
  reverse_methods <- methods_to_test[!methods_to_test %in% c('census')]
  
  for (method in reverse_methods) {
    print(paste0('Reverse single queries: ', method))
    result1 <- reverse_geo(lat = sample_lat, long = sample_long, method = method, full_results = TRUE)
    
    # check that a 1 row dataframe is returned
    expect_true(is.data.frame(result1))
    expect_equal(nrow(result1), 1)
    
    # check lat long
    expect_equal(result1$lat, sample_lat)
    expect_equal(result1$long, sample_long)
    
    # check address datatype
    expect_true(is.character(result1$address))
    
    # check that address returned was not NA
    expect_false(is.na(result1$address))
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
    print(paste0('Reverse batch queries: ', method))
    result1 <- reverse_geo(lat = sample_lats, long = sample_longs, method = method, full_results = TRUE)
    
    # check that a 1 row dataframe is returned
    expect_true(is.data.frame(result1))
    expect_equal(nrow(result1), length(sample_lats))
    
    # check lat long
    expect_equal(result1$lat, sample_lats)
    expect_equal(result1$long, sample_longs)
    
    # check address datatypes
    expect_true(all(is.character(result1$address)))
    
    # check that addresses returned are not NA
    expect_false(any(is.na(result1$address)))
  }
})

# test error catching for a single method
test_error_catching <- function(method, generic_args = list(), custom_args = list()) {
  print(paste0('Invalid query handling: ', method))
  expect_warning(result_w_error <- jsonlite::fromJSON(query_api(tidygeocoder:::get_api_url(method), 
                query_parameters = tidygeocoder::get_api_query(method, generic_args, custom_args))))
  expect_message(check_results <- tidygeocoder:::check_results_for_problems(method, result_w_error, FALSE))
  expect_true(check_results)
}

# Pass an invalid query to each service and make sure
# error messages are caught
test_that("test error catching", {
    test_error_catching('osm', list(format = 'invalid'))
    test_error_catching('census', list())
    
    # for methods requiring an api key, passing an invalid key
    # should cause an error
    for (method in setdiff(methods_to_test, c('osm', 'census'))) {
    test_error_catching(method, list(api_key = 'invalid'))
    }
})