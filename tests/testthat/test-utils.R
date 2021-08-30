test_that("Check API Parameter Reference Dataset", {
  
  # the api_name and method parameters should never be NA or blank
  expect_true(!any(is.na(tidygeocoder::api_parameter_reference$api_name)))
  expect_true(!any(is.na(tidygeocoder::api_parameter_reference$method)))
  
  # check for duplicates
  unique_api_param_rows <- nrow(tidygeocoder::api_parameter_reference[c('method', 'generic_name')])
  api_param_rows <- nrow(tidygeocoder::api_parameter_reference)
  
  expect_equal(unique_api_param_rows, api_param_rows)
})


# check that batch functions all exist and have a batch_limit default value
test_that('check batch function maps', {

  for (method in names(tidygeocoder:::batch_func_map)) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    expect_true(method %in% tidygeocoder::batch_limit_reference[['method']], label = method_label)
    expect_true(method %in% tidygeocoder::api_parameter_reference[['method']], label = method_label)
    expect_true(is.function(tidygeocoder:::batch_func_map[[method]]), label = method_label)
  }
  
  for (method in names(tidygeocoder:::reverse_batch_func_map)) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    expect_true(method %in% tidygeocoder::batch_limit_reference[['method']], label = method_label)
    expect_true(method %in% tidygeocoder::api_parameter_reference[['method']], label = method_label)
    expect_true(is.function(tidygeocoder:::reverse_batch_func_map[[method]]), label = method_label)
  }
  
})


test_that('Check Reference Datasets', {
  # Services that require an API key should have an api_key parameter
  # defined in api_parameter_reference and also have an environmental variable 
  # (where the key value will be read from) declared in api_key_reference
  expect_setequal(get_services_requiring_key(), tidygeocoder::api_key_reference[['method']])
  
  # make sure these datasets are unique on 'method'
  expect_equal(
    length(unique(tidygeocoder::api_key_reference[['method']])), 
    nrow(tidygeocoder::api_key_reference)
  )
  expect_equal(
    length(unique(tidygeocoder::batch_limit_reference[['method']])), 
    nrow(tidygeocoder::batch_limit_reference)
  )
  expect_equal(
    length(unique(tidygeocoder::min_time_reference[['method']])), 
    nrow(tidygeocoder::min_time_reference)
  )
})


# ---------------------------------------------------------------------------------------

# Check the package_addresses() and unpackage_inputs() functions
# with some duplicate addresses
test_that("Test Duplicate and Blank/NA Address Handling", {
  
  messy_addresses <- c('','', NA, tidygeocoder::sample_addresses$addr, 
      NA, '', NA, tidygeocoder::sample_addresses$addr)
  
  addr_pack <- tidygeocoder:::package_addresses(address = messy_addresses)
  
  # create NA lat long fields
  results <- tidygeocoder::geo(messy_addresses, no_query = TRUE, unique_only = TRUE)[, c('lat', 'long')]
  
  unpacked <- tidygeocoder:::unpackage_inputs(addr_pack, results, return_inputs = TRUE)
  
  # check data types and lengths
  expect_true(is.list(addr_pack))
  expect_length(addr_pack, 2)
  expect_true(tibble::is_tibble(addr_pack$unique))
  expect_true(tibble::is_tibble(addr_pack$crosswalk))
  
  # check number of rows in datasets
  expect_equal(nrow(addr_pack$unique), length(unique(messy_addresses[!(messy_addresses %in% c(NA, ''))])))
  expect_equal(nrow(addr_pack$crosswalk), length(messy_addresses))
  expect_equal(nrow(unpacked), length(messy_addresses))
  
  # check output column names
  expect_equal(colnames(unpacked), c('address', colnames(results)))
  
})

# make sure the address handler throws errors when it should
test_that("Test Address Packaging Errors", {

  # mixing 'address' argument with address component arguments
  expect_error(tidygeocoder:::package_addresses(address = c('xyz'), city = c('abc')))
  # mismatched lengths of arguments
  expect_error(tidygeocoder:::package_addresses(city = c('xyz', 'abc'), state = c('abc')))
})

test_that("Test API Query Creation Functions", {
  
  cust_arg_list <- list(cust1 = '123')
  address_val <- '1500 Rushmore St'
  
  # proper census parameter
  census_addr_param <- create_api_parameter('census', 'address', 'xyz')
  expect_true(is.list(census_addr_param))
  expect_equal(length(census_addr_param), 1)
  
  # improper census parameter. expect an empty list output
  census_bad_addr_param <- create_api_parameter('census', 'country', 'xyz')
  expect_true(is.list(census_bad_addr_param))
  expect_equal(length(census_bad_addr_param), 0)
  
  
  # loop through all methods and produce queries
  for (method in unique(tidygeocoder::api_parameter_reference[['method']])) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    # test overlap between generic and custom parameters
    expect_warning(tidygeocoder::get_api_query(method,
       generic_parameters = list(address = 'abc'),
       custom_parameters = tidygeocoder:::create_api_parameter(method, 'address', 'ghj')))
    
    
    default_q <- tidygeocoder::get_api_query(method)
    custom_q <- tidygeocoder::get_api_query(method, custom_parameters = cust_arg_list)
    address_q <- tidygeocoder::get_api_query(method, generic_parameters = list(address = address_val))

    expect_named(custom_q)
    expect_named(address_q)
    
    # all outputs should be lists
    expect_true(is.list(default_q))
    expect_true(is.list(custom_q))
    expect_true(is.list(address_q))
    
    # custom_parameters and generic_parameters arguments should just be 
    # adding to the default_q list
    expect_mapequal(custom_q, c(default_q, cust_arg_list))
    
    # for mapbox/tomtom the address is removed from parameters and put 
    # into the API URL so this test doesn't apply
    if (!method %in% c('mapbox', 'tomtom')) {
      expect_mapequal(address_q, c(default_q, 
        tidygeocoder:::create_api_parameter(method, 'address', address_val)))
    }
    
    if (length(default_q) > 0) expect_message(display_named_list(default_q), label = method_label)
    expect_message(display_named_list(custom_q), label = method_label)
    if (length(address_q) > 0) expect_message(display_named_list(address_q), label = method_label)
  }
})

## Miscellaneous functions

test_that("Test Miscellaneous Functions", {
  
  # na value function for return NA data
  num_rows <- 3
  na_vals <- tidygeocoder:::get_na_value('lat', 'long', rows = num_rows)
  expect_true(tibble::is_tibble(na_vals))
  expect_true(nrow(na_vals) == num_rows)
  
})
