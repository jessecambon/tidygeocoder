
dup_addresses <- rep(sample_addresses$addr, 2)

addr_pack <- tidygeocoder:::package_addresses(address = dup_addresses)

results <- tidygeocoder::geo(dup_addresses, no_query = TRUE)[, c('lat', 'long')]

unpacked <- tidygeocoder:::unpackage_addresses(addr_pack, results, return_addresses = TRUE)
