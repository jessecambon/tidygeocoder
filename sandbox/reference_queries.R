bh1 <- geo(sample_addresses$addr, method = 'here', mode = 'batch', verbose = TRUE, full_results = TRUE)

# NOTE - took 22 seconds
bhr1 <- reverse_geo(lat = louisville$latitude[1:3], long = louisville$longitude[1:3], 
                    method = 'here', mode = 'batch', verbose = TRUE, full_results = TRUE)

### Test queries
a <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'osm', verbose = TRUE)
b <- reverse_geo(lat = 38.895865, long = -77.0307713, method = 'google', full_results = TRUE, verbose = TRUE)

c <- reverse_geo(lat = c(38.895865, 43.6534817, 300), long = c(-77.0307713, -79.3839347, 600), method = 'geocodio', full_results = TRUE, verbose = TRUE)

bq1 <- reverse_geocode(tibble::tibble(latitude = c(38.895865, 43.6534817, 700), longitude = c(-77.0307713, -79.3839347, 300)), 
  lat = latitude, long = longitude, method = 'osm', full_results = TRUE, verbose = TRUE)

bl2 <- reverse_geocode(tibble::tibble(latitude = c(38.895865, 43.6534817, 35.0844), longitude = c(-77.0307713, -79.3839347, 106.6504)), 
 lat = latitude, long = longitude, method = 'geocodio', full_results = TRUE, verbose = TRUE, batch_limit = 1)

d <- reverse_geocode(tibble(latitude = c(38.895865, 43.6534817), longitude = c(-77.0307713, -79.3839347)), 
 lat = latitude, long = longitude, method = 'osm', full_results = TRUE, verbose = TRUE)