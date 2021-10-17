# Test geocode_combine() and geo_combine()

library(tibble)

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
  
})

test_that("Test geocode_combine() list output", {
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

test_that("Test geo_combine()", {
  
  example_addresses <- c("100 Main St New York, NY", "Paris", "Not a Real Address")

  expect_true(is_tibble(
  result1 <- geo_combine(queries = list(list(method = 'census'), list(method = 'osm')),
    address = example_addresses,
    global_params = list(address = 'address', no_query = TRUE),
    lat = latitude, long = longitude
    )))
  
  # check that naming of lat and long columns works
  expect_setequal(
    colnames(result1),
    c('address', 'latitude', 'longitude', 'query')
  )
  
  # check that number of rows is preserved
  expect_equal(
    nrow(result1),
    length(example_addresses)
  )
  
})


