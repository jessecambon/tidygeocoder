## Test geocoding functionality without making any API calls

# Make sure there are no duplicates in our API reference files
test_that("Check API Parameter Reference For Duplicates", {

  # Check that generic_name values are unique for each method
  method_args <- tidygeocoder::api_parameter_reference[c('method', 'generic_name')]
  # remove NA generic_names 
  method_args <- method_args[!is.na(method_args$generic_name), ]
  
  expect_equal(nrow(method_args), nrow(unique(method_args)))
})

# Check column names with custom settings
test_that("geocode default colnames", {
  result <- tibble::tibble(addr = NA) %>%
    geocode(addr, no_query = TRUE)

  expect_identical(colnames(result) ,c('addr','lat','long'))
  expect_equal(nrow(result), 1) # result should have one row
})

# Check column names with custom settings
test_that("geocode custom colnames", {
  result <- tibble::tibble(addr = '')  %>%
    geocode(addr, lat = 'latitude', long = 'longitude', no_query = TRUE)

  expect_identical(colnames(result), c('addr', 'latitude', 'longitude'))
  expect_equal(nrow(result), 1) # result should have one row
})

# Check that null/empty address values are handled properly
test_that("geocode null/empty addresses", {
  NA_result <- get_na_value('lat', 'long')
  
  # make sure blank addresses are not being sent to the geocoder
  expect_identical(geo_census(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  expect_identical(geo_osm(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  expect_identical(names(geo_cascade(" ", return_addresses = FALSE, no_query = TRUE)), 
                   c('lat', 'long', 'geo_method'))
  expect_identical(geo_google(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  
  # Test with tibble
  NA_data <- tibble::tribble(~addr,
                             "   ",
                             NA,
                             "")

  result <- NA_data %>% geocode(addr, no_query = TRUE, method = 'osm')
  
  # check column names
  expected_colnames <- c(colnames(NA_data), 'lat', 'long')
  expect_identical(colnames(result), expected_colnames)
  expect_identical(colnames(geocode(NA_data, addr, method = 'google', no_query = TRUE)), expected_colnames)
  
  # make sure geo_method is NA when address is NA
  expect_equal(nrow(result), nrow(NA_data)) # check dataframe length
  expect_equal(nrow(geocode(NA_data, addr, method = 'google', no_query = TRUE)), nrow(NA_data))
  
  # Test batch limit
  expect_message(batch_limit_results1 <- geo(address = as.character(seq(1, 10)), 
                method = 'census', batch_limit = 5, no_query = TRUE))
  expect_equal(10, nrow(batch_limit_results1))
  
  expect_message(geo(address = as.character(seq(1, 10)), 
     method = 'census', batch_limit = 5, no_query = TRUE, unique_only = TRUE))
})

test_that("Test geo() error handling", {
  
  # invalid cascade_order
  expect_error(geo(no_query = TRUE, address = 'abc', method = 'cascade', cascade_order = 1))
  # invalid method
  expect_error(geo(no_query = TRUE, address = 'abc', method = '123'))
  # invalid mode
  expect_error(geo(no_query = TRUE, address = 'abc', mode = '123'))
  # incompatible address arguments
  expect_error(geo(no_query = TRUE, address = 'abc', street = 'xyz', no_query = TRUE)) 
  # invalid return_type
  expect_error(geo(no_query = TRUE, address = 'abc', return_type = 'xyz')) 
  # invalid limit
  expect_error(geo(no_query = TRUE, address = 'abc', limit = 0)) 
  # don't allow mixed lengths for address components
  expect_error(geo(no_query = TRUE, city = c('x', 'y'), state = 'ab'))
  
  # invalid parameters for the census service (country and limit != 1)
  expect_error(geo('yz', no_query = TRUE, country = 'abc', method = 'census'))
  expect_error(geo('yz', no_query = TRUE, limit = 5, method = 'census'))
  
  # improper limit value for census but param_error = FALSE and verbose = TRUE so we expect a message
  expect_message(geo('yz', no_query = TRUE, limit = 5, method = 'census', verbose = TRUE, param_error = FALSE))
  
  # improper parameters for cascade (limit !=1 and full_results = TRUE)
  expect_error(geo('xy', no_query = TRUE, full_results = TRUE, method = 'cascade'))
  expect_error(geo('ab', no_query = TRUE, limit = 5, method = 'cascade'))
  
  
})

test_that("Test geocode() error handling", {
  addr_df <- tibble::tibble(addr = 'xyz')
  named_list <- list(addr = 'xyz')
  
  # expect error when using wrong column name
  expect_error(geocode(addr_df, no_query = TRUE, address = wrong))
  expect_error(geocode(addr_df, no_query = TRUE, address = "wrong"))
  
  # non-dataframe input
  expect_error(geocode(named_list, no_query = TRUE, address = 'addr'))
})