test_that("Check API Parameter Reference For Duplicates", {
  
  unique_api_param_rows <- nrow(tidygeocoder::api_parameter_reference[c('method','generic_name')])
  api_param_rows <- nrow(tidygeocoder::api_parameter_reference)
  
  expect_equal(unique_api_param_rows,api_param_rows)
})


# Check the package_addresses() and unpackage_addresses() functions
# with some duplicate addresses
test_that("Test Duplicate Address Handling", {
  
  # duplicate addresses
  dup_addresses <- rep(sample_addresses$addr, 2)
  
  addr_pack <- tidygeocoder:::package_addresses(address = dup_addresses)
  
  # create NA lat lng fields
  results <- tidygeocoder::geo(dup_addresses, no_query = TRUE)[, c('lat', 'long')]
  
  unpacked <- tidygeocoder:::unpackage_addresses(addr_pack, results, return_addresses = TRUE)
  
  # check data types and lengths
  expect_true(is.list(addr_pack))
  expect_length(addr_pack, 2)
  expect_true(tibble::is_tibble(addr_pack$unique))
  expect_true(tibble::is_tibble(addr_pack$crosswalk))
  
  # check number of rows in datasets
  expect_equal(nrow(addr_pack$unique), nrow(sample_addresses))
  expect_equal(nrow(addr_pack$crosswalk), length(dup_addresses))
  expect_equal(nrow(unpacked), length(dup_addresses))
  
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
  
  for (method in c('google', 'census', 'osm', 'geocodio', 'iq')) {
    
    # test overlap between generic and custom parameters
    expect_error(tidygeocoder::get_api_query(method,
       generic_parameters = list(address = 'abc'),
       custom_parameters = tidygeocoder:::create_api_parameter(method, 'address', 'ghj')))
    
    cust_arg_list <- list(cust1 = '123')
    address_val <- '1500 Rushmore St'
    
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
    expect_mapequal(address_q, c(default_q, 
      tidygeocoder:::create_api_parameter(method, 'address', address_val)))
  }
})