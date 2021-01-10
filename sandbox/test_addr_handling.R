
dup_addresses <- c(c('','', NA), sample_addresses$addr, c(NA, '', NA), sample_addresses$addr)

addr_pack <- tidygeocoder:::package_addresses(address = dup_addresses)

results <- tidygeocoder::geo(dup_addresses, no_query = TRUE, unique_only = TRUE)[, c('lat', 'long')]

unpacked <- tidygeocoder:::unpackage_addresses(addr_pack, results, return_addresses = TRUE)
