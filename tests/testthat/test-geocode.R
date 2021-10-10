## Test geocoding functionality without making any API calls

library(tibble)

# get all method names
all_methods <- unique(tidygeocoder::api_parameter_reference[['method']])

# Check column names with custom settings
test_that("geocode default colnames", {
  result <- tibble::tibble(addr = NA) %>%
    geocode(addr, no_query = TRUE)
  
  expect_identical(colnames(result), c('addr', 'lat', 'long'))
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
  # expected NA result
  NA_result <- get_na_value('lat', 'long')
  
  # NA input data
  NA_data <- tibble::tribble(~addr,
                             "   ",
                             NA,
                             "")
  
  expected_colnames <- c(colnames(NA_data), 'lat', 'long')
  
  for (method in all_methods) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    # make sure blank addresses are not being sent to the geocoder
    expect_identical(
      geo(" ", method = method, return_addresses = FALSE, no_query = TRUE), 
      NA_result, label = method_label
    )
    
    # test geocoding NA/blank data
    result <- NA_data %>% geocode(addr, no_query = TRUE, method = method)
    
    # check column names
    expect_identical(colnames(result), expected_colnames, label = method_label)
    
    # make sure geo_method is NA when address is NA
    expect_equal(nrow(result), nrow(NA_data), label = method_label) # check dataframe length
    
  }
})

test_that("Test geo() and reverse_geo() error handling", {
  
  # invalid cascade_order
  #expect_error(geo(no_query = TRUE, address = 'abc', method = 'cascade', cascade_order = 1))
  # invalid method
  expect_error(geo(no_query = TRUE, address = 'abc', method = '123'))
  expect_error(reverse_geo(no_query = TRUE, lat = 1, long = 2, method = '123'))
  # invalid mode
  expect_error(geo(no_query = TRUE, address = 'abc', mode = '123'))
  expect_error(reverse_geo(no_query = TRUE, lat = 1, long = 2, mode = '123'))
  # incompatible address arguments
  expect_error(geo(no_query = TRUE, address = 'abc', street = 'xyz', no_query = TRUE)) 
  # invalid return_type
  expect_error(geo(no_query = TRUE, address = 'abc', api_options = list(census_return_type = 'xyz'))) 
  
  # invalid api_option argument
  expect_error(geo(no_query = TRUE, address = 'abc', api_options = list(bad_argument = 'xyz'))) 
  
  # invalid limit
  expect_error(geo(no_query = TRUE, address = 'abc', limit = 0)) 
  expect_error(reverse_geo(no_query = TRUE, lat = 1, long = 2, limit = 0))
  # don't allow mixed lengths for inputs
  expect_error(geo(no_query = TRUE, city = c('x', 'y'), state = 'ab'))
  expect_error(reverse_geo(no_query = TRUE, lat = c(1,5), long = 2))
  
  
  # should not allow batch geocoding with a method that doesn't have batch geocoding
  expect_error(geo('yz', no_query = TRUE, mode = 'batch', method = 'osm'))
  expect_error(reverse_geo(lat = 1, long = 2, no_query = TRUE, mode = 'batch', method = 'osm'))
  
  # invalid parameters for the census service (country and limit != 1)
  expect_error(geo('yz', no_query = TRUE, country = 'abc', method = 'census'))
  
  # improper parameters for cascade (limit !=1 and full_results = TRUE)
  # expect_error(geo('xy', no_query = TRUE, full_results = TRUE, method = 'cascade'))
  # expect_error(geo('ab', no_query = TRUE, limit = 5, method = 'cascade'))
  
  # invalid mapbox_permanent parameter
  expect_error(geo(no_query = TRUE, address = 'abc', api_options = list(mapbox_permanent = "AA")))
  
  # invalid api_options parameter
  expect_error(geo(no_query = TRUE, address = 'abc', api_options = list(invalid_parameter = "blah")))
  
  # invalid here_request_id parameter
  expect_error(geo(no_query = TRUE, address = 'abc', method = 'here', api_options = list(here_request_id  = 12345)))
  expect_error(reverse_geo(no_query = TRUE, lat = 1, long = 2, method = 'here', api_options = list(here_request_id  = 12345)))
  
  # here specific batch issue for here_request_id
  expect_error(reverse_geo(no_query = TRUE, lat = c(0,1), long = c(0,0), 
      method = 'here', api_options = list(here_request_id = 'asdf'), return_coords = TRUE, mode = 'batch'))
  expect_error(geo(no_query = TRUE, address = c('xyz', 'abc'),
      method = 'here', api_options = list(here_request_id = 'asdf'), return_addresses = TRUE, mode = 'batch'))
  
  
  # Test batch limit detection and error/warning toggling - geo()
  expect_error(geo(address = as.character(seq(1, 10)), 
                   method = 'census', batch_limit = 5, no_query = TRUE))
  expect_warning(geo(address = as.character(seq(1, 10)), 
                     method = 'census', batch_limit = 5, no_query = TRUE, batch_limit_error = FALSE))
  
  # Test reverse_geo() batch limit handling
  expect_error(reverse_geo(lat = c(1,2,3), long = c(0,0,0),
                   method = 'geocodio', batch_limit = 2, no_query = TRUE))
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


# Check that null/empty address values are handled properly
test_that("reverse geocode null/empty addresses", {
  # Currently census is the only method that doesn't support reverse geocoding
  reverse_methods <- all_methods[!all_methods %in% pkg.globals$no_reverse_methods]
  
  NA_result <- tibble::tibble(address = as.character(NA))
  NA_data <- tibble::tribble(~lat,~lon,
                             as.numeric(NA), as.numeric(NA),
                             5000, 5000)
  expected_colnames <- c(colnames(NA_data), 'address')

  for (method in reverse_methods) {
    # label to include in error message so we know which method failed
    method_label = paste0('method = "', method, '"', ' ')
    
    # check colnames
    expect_identical(colnames(reverse_geocode(NA_data, lat = lat, long = lon,
                              method = method, no_query = TRUE)), expected_colnames, label = method_label)
    # check dataframe length
    expect_equal(nrow(reverse_geocode(NA_data, lat = lat, long = lon, 
                          method = method, no_query = TRUE)), nrow(NA_data), label = method_label)
    # check NA results on blank
    expect_identical(reverse_geo(lat = " ", long = " ", method = method, 
                                 return_coords = FALSE, no_query = TRUE), NA_result, label = method_label)
  }
})


test_that("Test limit related error handling", {
  addr_input <- tibble(addr = c('zyx', 'etd'))
  coord_input <- tibble(lat = c(1, 2), long = c(4, 5))
  
  # reverse_geo() 
  expect_error(reverse_geo(no_query = TRUE, method = 'geocodio', lat = 1, long = 2, mode = 'batch', limit = 5))
  expect_error(reverse_geo(no_query = TRUE, method = 'geocodio', lat = 1, long = 2, mode = 'batch', limit = NULL))
  expect_true(is_tibble(reverse_geo(no_query = TRUE, method = 'geocodio', lat = 1, long = 2, mode = 'batch', limit = 1)))
  expect_true(is_tibble(reverse_geo(no_query = TRUE, method = 'geocodio', lat = 1, long = 2, mode = 'batch', limit = 5, return_coords = FALSE)))
  
  # geo()
  expect_error(geo(no_query = TRUE, method = 'geocodio', address = 'xyz', mode = 'batch', limit = 5))
  expect_error(geo(no_query = TRUE, method = 'geocodio', address = 'xyz', mode = 'batch', limit = NULL))
  expect_true(is_tibble(geo(no_query = TRUE, method = 'geocodio', address = 'xyz', mode = 'batch', limit = 1)))
  expect_true(geo(no_query = TRUE, method = 'geocodio', address = 'xyz', mode = 'batch', limit = 5, return_addresses = FALSE) %>% is_tibble())
  
  # geocode()
  expect_error(geocode(addr_input, address = addr, no_query = TRUE, method = 'osm', limit = 5, return_input = TRUE))
  expect_error(geocode(addr_input, address = addr, no_query = TRUE, method = 'osm', limit = NULL, return_input = TRUE))
  expect_true(is_tibble(geocode(addr_input, address = addr, no_query = TRUE, method = 'osm', return_input = TRUE)))
  expect_true(is_tibble(geocode(addr_input, address = addr, no_query = TRUE, method = 'osm', unique_only = TRUE)))
  
  # reverse_geocode()
  expect_error(reverse_geocode(coord_input, lat = lat, long = long, no_query = TRUE, method = 'osm', return_input = TRUE, limit = 5))
  expect_error(reverse_geocode(coord_input, lat = lat, long = long, no_query = TRUE, method = 'osm', return_input = TRUE, limit = NULL))
  expect_true(reverse_geocode(coord_input, lat = lat, long = long, no_query = TRUE, method = 'osm', return_input = FALSE) %>% is_tibble())
  expect_true(reverse_geocode(coord_input, lat = lat, long = long, no_query = TRUE, method = 'osm', unique_only = TRUE) %>% is_tibble())
  
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


test_that("Test geocode_combine()", {
  # test that output is a tibble
  expect_true(is_tibble(
    na_results <- geocode_combine(sample_addresses, 
        queries = list(list(method = 'osm'), list(method = 'arcgis')),
        global_params = list(address = 'addr', no_query = TRUE))
    ))
  
  # make sure all input address column names are included in output
  expect_equal(
    length(intersect(colnames(sample_addresses), colnames(na_results))),
    length(colnames(sample_addresses))
  )
  
  # Make sure number of rows is preserved in the output
  expect_equal(nrow(na_results), nrow(sample_addresses))
  
  # method error handling
  expect_error(
    geocode_combine(sample_addresses, list(list(method = 'invalid_method'), list(method = 'arcgis')),
                    global_params = list(address = 'addr', no_query = TRUE))
  )
  
  # global_params error handling
  expect_error(
    geocode_combine(sample_addresses, list(list(method = 'osm'), list(method = 'arcgis')),
                    global_params = list(address = 'addr', bad_argument = TRUE, no_query = TRUE))
  )
  
  # bad queries argument
  expect_error(
    geocode_combine(sample_addresses, 'blarg',
                    global_params = list(address = 'addr', no_query = TRUE))
  )
  
  # Duplicate query names
  expect_error(
    geocode_combine(sample_addresses, list(list(method = 'osm'), list(method = 'arcgis')),
                    query_names = c('duplicate', 'duplicate'),
                    global_params = list(address = 'addr', no_query = TRUE))
  )
  
  # Invalid number of query names
  expect_error(
    geocode_combine(sample_addresses, list(list(method = 'osm'), list(method = 'arcgis')),
                    query_names = c('only one name'),
                    global_params = list(address = 'addr', no_query = TRUE))
  )
  
  # Invalid dataframe argument
  expect_error(
    geocode_combine(list(x = 1), list(list(method = 'osm'), list(method = 'arcgis')),
                    global_params = list(address = 'addr', no_query = TRUE))
  )
  
  # Invalid address argument
  expect_error(
    geocode_combine(sample_addresses, list(list(method = 'osm'), list(method = 'arcgis')),
                    global_params = list(address = 'bad column name', no_query = TRUE))
  )
  
  # Test list output -------------------------------------------------------------------------
  expect_true(is.list(
    na_results_list <- geocode_combine(sample_addresses, 
                  return_list = TRUE,
                  queries = list(list(method = 'census'), list(method = 'osm')),
                  global_params = list(address = 'addr', no_query = TRUE))
  ))
  
  # all items in list should be a tibble
  expect_true(all(sapply(na_results_list, is_tibble)))
  
  # check list names
  expect_setequal(
    names(na_results_list),
    c('census', 'osm', '')
  )
  
  # check that all results are categorized as not found (last item in the list)
  expect_equal(nrow(na_results_list[[3]]), nrow(sample_addresses))
  
  # check column names in NA output
  expect_setequal(colnames(na_results_list[[3]]), colnames(sample_addresses))
  
  
  
})

