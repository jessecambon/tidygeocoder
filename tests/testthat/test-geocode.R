## Test geocoding functionality without making any API calls

# Check column names with custom settings
test_that("geocode default colnames", {
  result <- tibble::tibble(addr = NA) %>%
    geocode(addr, no_query = TRUE)
  
  expect_identical(colnames(result), c('addr','lat','long'))
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
  expect_identical(geo_opencage(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  expect_identical(geo_mapbox(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  expect_identical(geo_here(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  expect_identical(geo_tomtom(" ", return_addresses = FALSE, no_query = TRUE), NA_result)
  
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
  expect_identical(colnames(geocode(NA_data, addr, method = 'opencage', no_query = TRUE)), expected_colnames)
  
  # make sure geo_method is NA when address is NA
  expect_equal(nrow(result), nrow(NA_data)) # check dataframe length
  expect_equal(nrow(geocode(NA_data, addr, method = 'google', no_query = TRUE)), nrow(NA_data))
  expect_equal(nrow(geocode(NA_data, addr, method = 'opencage', no_query = TRUE)), nrow(NA_data))
  
  # Test batch limit detection and error/warning toggling
  expect_error(geo(address = as.character(seq(1, 10)), 
                   method = 'census', batch_limit = 5, no_query = TRUE, batch_limit_error = TRUE))
  expect_warning(geo(address = as.character(seq(1, 10)), 
                     method = 'census', batch_limit = 5, no_query = TRUE, batch_limit_error = FALSE))
  # batch_limit_error should revert to FALSE with method = 'cascade'
  expect_warning(geo(address = as.character(seq(1, 10)), 
                     method = 'cascade', batch_limit = 5, no_query = TRUE, batch_limit_error = TRUE))
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
  expect_warning(geo('yz', no_query = TRUE, limit = 5, method = 'census', verbose = TRUE, param_error = FALSE))
  
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

#### Reverse geocoding


# Check that null/empty address values are handled properly
test_that("reverse geocode null/empty addresses", {
  NA_result <- tibble::tibble(address = as.character(NA))
  
  # make sure blank addresses are not being sent to the geocoder
  expect_identical(reverse_geo(lat = " ", long = " ", method = 'osm', return_coords = FALSE, no_query = TRUE), NA_result)
  expect_identical(reverse_geo(lat =" ", long = " ", method = 'google', return_coords = FALSE, no_query = TRUE), NA_result)
  expect_identical(reverse_geo(lat = " ", long = " ", method = 'opencage', return_coords = FALSE, no_query = TRUE), NA_result)
  expect_identical(reverse_geo(lat = " ", long = " ", method = 'mapbox', return_coords = FALSE, no_query = TRUE), NA_result)
  expect_identical(reverse_geo(lat = " ", long = " ", method = 'here', return_coords = FALSE, no_query = TRUE), NA_result)
  expect_identical(reverse_geo(lat = " ", long = " ", method = 'tomtom', return_coords = FALSE, no_query = TRUE), NA_result)
  
  
  # Test with tibble
  NA_data <- tibble::tribble(~lat,~lon,
                             as.numeric(NA), as.numeric(NA),
                             5000, 5000)
  
  result <- NA_data %>% reverse_geocode(lat = lat, long = lon, no_query = TRUE, method = 'osm')
  
  # check column names
  expected_colnames <- c(colnames(NA_data), 'address')
  expect_identical(colnames(result), expected_colnames)
  expect_identical(colnames(reverse_geocode(NA_data, lat = lat, long = lon, method = 'google', no_query = TRUE)), expected_colnames)
  expect_identical(colnames(reverse_geocode(NA_data, lat = lat, long = lon, method = 'opencage', no_query = TRUE)), expected_colnames)
  
  # make sure geo_method is NA when address is NA
  expect_equal(nrow(result), nrow(NA_data)) # check dataframe length
  expect_equal(nrow(reverse_geocode(NA_data, lat = lat, long = lon, method = 'google', no_query = TRUE)), nrow(NA_data))
  expect_equal(nrow(reverse_geocode(NA_data, lat = lat, long = lon, method = 'opencage', no_query = TRUE)), nrow(NA_data))
  
  # Test batch limit
  
  # expect_message(batch_limit_results1 <- geo(address = as.character(seq(1, 10)), 
  #                                            method = 'census', batch_limit = 5, no_query = TRUE))
  # expect_equal(10, nrow(batch_limit_results1))
  # 
  # expect_message(geo(address = as.character(seq(1, 10)), 
  #                    method = 'census', batch_limit = 5, no_query = TRUE, unique_only = TRUE))
})



test_that("Test reverse_geocode() error handling", {
  addr_df <- tibble::tibble(lat = 1, long = 2)
  named_list <- list(lat = 1, long = 2)
  
  # expect error when using wrong column name
  expect_error(reverse_geocode(addr_df, no_query = TRUE, lat = lat, long = wrong))
  expect_error(reverse_geocode(addr_df, no_query = TRUE, lat = wrong, long = long))
  expect_error(reverse_geocode(addr_df, no_query = TRUE, lat = wrong1, long = wrong2))
  
  # non-dataframe input
  expect_error(reverse_geocode(named_list, no_query = TRUE, address = 'addr'))
})