
# this works
test1 <- geo(sample_addresses$addr, method = 'osm', limit = 5, full_results = TRUE, verbose = TRUE)


# osm doesn't have a limit parameter in reverse
rev1 <- reverse_geo(lat = louisville$latitude[1:5], long = louisville$longitude[1:5], limit = 5, 
                    method = 'osm', verbose = TRUE, full_results = TRUE)

# geocodio - this also seems to work
rev_gc1 <- reverse_geo(lat = louisville$latitude[1:5], long = louisville$longitude[1:5], limit = 5, 
                    method = 'geocodio', verbose = TRUE, full_results = TRUE, mode = 'single')

# google - seems to work as well
rev_goog1 <- reverse_geo(lat = louisville$latitude[1:5], long = louisville$longitude[1:5], limit = 5, 
                    method = 'google', verbose = TRUE, full_results = TRUE)
