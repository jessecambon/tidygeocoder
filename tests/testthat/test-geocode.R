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
