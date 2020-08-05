# prove out methodology for cascade function that works
# with batch geocoding

input <-list(street=c("123 Main St", "", "45 Market St"),
             city=c("Pawnee", "Toronto", "Louisville"),
             state=c("IN", "", "KY"))

census_results <- tibble::tribble(
  ~lat, ~long,
  12, -14,
   NA, NA,
   45, 3)

# find NA result indices for census
na_indices <- is.na(census_results[['lat']]) | is.na(census_results[['long']])

# this is what we give to osm
retry_input <- lapply(input, function(x, bools) x[bools], na_indices)

# osm results
retry_results <- tibble::tribble(
  ~lat, ~long,
  3, -20)

## combine census and retry results
combi <- census_results
combi[na_indices,] <- retry_results

combi$geo_method <- ifelse(na_indices, 'osm', 'census')
