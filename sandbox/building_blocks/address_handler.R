
# demo how addresses could be processed
package_addresses <- function(address = NULL, city = NULL, state = NULL) {
  combined_addr <- as.list(environment())
  combined_addr[sapply(combined_addr, is.null)] <- NULL # remove NULL items
  
  # put original addresses in a tibble
  addr_orig <- tibble::as_tibble(combined_addr)
  addr_col_names <- names(addr_orig)
  
  # dedup addresses
  unique_addr <- unique(addr_orig)
  unique_addr[['.uid']] <- 1:nrow(unique_addr)
  
  # create id to record original address order
  addr_orig[['.id']] <- 1:nrow(addr_orig) 
  
  # crosswalk
  crosswalk <- merge(addr_orig, unique_addr, by = addr_col_names, all.x = TRUE, sort = FALSE)
  crosswalk <- crosswalk[order(crosswalk['.id']),]  # reorder
  
  return(list(unique = unique_addr, crosswalk = subset(crosswalk, select = -.id)))
}

a <- package_addresses(c('a','f','f','z','d','z','a'), state = c('KY','NY','NY','ME','AL','AL','KY'))