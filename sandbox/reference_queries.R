bh1 <- geo(sample_addresses$addr, method = 'here', mode = 'batch', verbose = TRUE, full_results = TRUE)

# NOTE - took 22 seconds
bhr1 <- reverse_geo(lat = louisville$latitude[1:3], long = louisville$longitude[1:3], 
                    method = 'here', mode = 'batch', verbose = TRUE, full_results = TRUE)