#' Function for packaging and deduping addresses
#' 
#' @export
package_addresses <- function(address = NULL, 
  street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL) {
  # package all non-NULL address arguments into a named list
  combined_addr <- as.list(environment())
  combined_addr[sapply(combined_addr, is.null)] <- NULL # remove NULL items
  
  
  arg_names <- names(combined_addr)
  
  if (('address' %in% arg_names) & (length(arg_names) > 1)) {
    warning("Do not use other address component parameters with the single line 'address' parameter")
    return(list())
  }
  
  address_component_lengths <- lengths(combined_addr)
  #print(address_component_lengths)
  if (max(address_component_lengths) != min(address_component_lengths)) {
    warning('Address components must be equal in length')
    return(list())
  }

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
  
  return(list(unique = tibble::as_tibble(subset(unique_addr, select = -.uid)), 
              crosswalk = tibble::as_tibble(crosswalk[!names(crosswalk) %in% addr_col_names])
  ))
}
#'
#' Function for unpackaging and RE-deduping addresses
#' so that we can return them in the same order that they were passed
#' 
#' @export
unpackage_addresses <- function(package, results) {
  
  #package$unique[['.uid']] <- 1:nrow(package$unique)
  results[['.uid']] <- 1:nrow(results)
  
  base <- merge(package$crosswalk, results, by = '.uid', all.x = TRUE, sort = FALSE)
  base <- base[order(base['.id']),]  # reorder
  
  return(tibble::as_tibble(base[!names(base) %in% c('.id','.uid')]))
}

### Test
# > library(tidygeocoder)
# > a <- package_addresses(city=c('Louisville', 'New Orleans', 'Louisville'), state = c('KY', 'LA', 'KY'))
# > results <- tibble::tibble(x= c(1,2), y= c(10,5))
# > unpackage_addresses(a, results)
# x  y
# 1 10
# 2  5
# 1 10