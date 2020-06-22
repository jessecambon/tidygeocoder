# Make sure there are no duplicates in our API reference files
test_that("Check API Parameter Reference For Duplicates", {
  
  unique_api_param_rows <- nrow(tidygeocoder::api_parameter_reference[c('method','generic_name')])
  api_param_rows <- nrow(tidygeocoder::api_parameter_reference)
  
  expect_equal(unique_api_param_rows,api_param_rows)
})

# Check column names with custom settings
test_that("geocode default colnames", {
  result <- tibble::tibble(addr = NA) %>%
    geocode(addr)

  expected_colnames <- c('addr','lat','long')

  expect_identical(colnames(result),expected_colnames)
  expect_equal(nrow(result),1) # result should have one row
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

  # Test with tibble
  NA_data <- tibble::tribble(~addr,
                             "   ",
                             NA,
                             "")

  result <- NA_data %>% geocode(addr, method = 'osm')
  
  # check column names
  expected_colnames <- c(colnames(NA_data), 'lat', 'long')
  expect_identical(colnames(result), expected_colnames)
  
  # make sure geo_method is NA when address is NA
  expect_equal(nrow(result), nrow(NA_data)) # check dataframe length
})

