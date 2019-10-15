# Check column names with custom settings
test_that("geocode default colnames", {
  result <- sample_addresses %>%
    dplyr::slice(1) %>% # limit dataset to one row
    geocode(addr)

  expected_colnames <- c(colnames(sample_addresses),'lat','long')

  expect_identical(colnames(result),expected_colnames)
  expect_equal(nrow(result),1) # result should have one row
})

# Check column names with custom settings
test_that("geocode custom colnames", {
  result <- sample_addresses %>%
    dplyr::slice(1) %>% # limit dataset to one row
    geocode(addr,lat='latitude',long='longitude')

  expected_colnames <- c(colnames(sample_addresses),'latitude','longitude')

  expect_identical(colnames(result),expected_colnames)
  expect_equal(nrow(result),1) # result should have one row
})

# Check that null/empty address values are handled properly
test_that("geocode null/empty addresses", {

  # make sure blank addresses are not being sent to the geocoder
  expect_message(geo_census(" ",verbose=TRUE),"Blank or missing address!")
  expect_message(geo_census(" 123 ",verbose=TRUE),"Blank or missing address!")
  expect_message(geo_osm(" ",verbose=TRUE),"Blank or missing address!")
  expect_message(geo_cascade(" ",verbose=TRUE),"Blank or missing address!")

  # Test with tibble
  NA_data <- tibble::tribble(~addr,
                             "   ",
                             NA,
                             "")

  result <- NA_data %>% geocode(addr,method='cascade')
  # check column names
  expected_colnames <- c(colnames(NA_data),'lat','long','geo_method')
  expect_identical(colnames(result),expected_colnames)
  # make sure geo_method is NA when address is NA
  expect_identical(is.na(result %>% dplyr::pull(geo_method)),rep(TRUE,nrow(NA_data)))
  expect_equal(nrow(result),nrow(NA_data)) # check dataframe length
})

