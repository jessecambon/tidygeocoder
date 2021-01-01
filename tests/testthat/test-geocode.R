<<<<<<< HEAD
# Make sure there are no duplicates in our API reference files
test_that("Check API Parameter Reference For Duplicates", {

  # Check that generic_name values are unique for each method
  method_args <- tidygeocoder::api_parameter_reference[c('method', 'generic_name')]
  # remove NA generic_names 
  method_args <- method_args[!is.na(method_args$generic_name), ]
  
  expect_equal(nrow(method_args), nrow(unique(method_args)))
})
=======
## Test geocoding functionality without making any API calls
>>>>>>> add-tests

# Check column names with custom settings
test_that("geocode default colnames", {
  result <- tibble::tibble(addr = NA) %>%
    geocode(addr)

  expected_colnames <- c('addr','lat','long')

  expect_identical(colnames(result),expected_colnames)
  expect_equal(nrow(result), 1) # result should have one row
})

# Check column names with custom settings
test_that("geocode custom colnames", {
  result <- tibble::tibble(addr = '')  %>%
    geocode(addr, lat = 'latitude', long = 'longitude')

  expected_colnames <- c('addr','latitude','longitude')

  expect_identical(colnames(result),expected_colnames)
  expect_equal(nrow(result),1) # result should have one row
})

# Check that null/empty address values are handled properly
test_that("geocode null/empty addresses", {
  NA_result <- get_na_value('lat', 'long')
  
  # make sure blank addresses are not being sent to the geocoder
  expect_identical(geo_census(" ", return_addresses = FALSE), NA_result)
  expect_identical(geo_osm(" ", return_addresses = FALSE), NA_result)
  expect_identical(names(geo_cascade(" ", return_addresses = FALSE)), 
                   c('lat', 'long', 'geo_method'))
  expect_identical(geo_google(" ", return_addresses = FALSE), NA_result)
  
  # Test with tibble
  NA_data <- tibble::tribble(~addr,
                             "   ",
                             NA,
                             "")

  result <- NA_data %>% geocode(addr, method = 'osm')
  
  # check column names
  expected_colnames <- c(colnames(NA_data), 'lat', 'long')
  expect_identical(colnames(result), expected_colnames)
  expect_identical(colnames(geocode(NA_data, addr, method = 'google')), expected_colnames)
  
  # make sure geo_method is NA when address is NA
  expect_equal(nrow(result), nrow(NA_data)) # check dataframe length
  expect_equal(nrow(geocode(NA_data, addr, method = 'google')), nrow(NA_data))
})

# Check column names with custom settings
test_that("Test geo() error handling", {
  
  # invalid cascade_order
  expect_error(geo(address = 'abc', no_query = TRUE, method = 'cascade', cascade_order = 1))
  # invalid method
  expect_error(geo(address = 'abc', no_query = TRUE, method = '123'))
  # invalid mode
  expect_error(geo(address = 'abc', no_query = TRUE, mode = '123'))
  # incompatible address arguments
  expect_error(geo(address = 'abc', street = 'xyz', no_query = TRUE)) 
  # invalid return_type
  expect_error(geo(address = 'abc', return_type = 'xyz', no_query = TRUE)) 
  # invalid limit
  expect_error(geo(address = 'abc', limit = 0, no_query = TRUE)) 
  
})
