# Function for packaging and deduping addresses
# 
# @export
package_addresses <- function(address = NULL, 
  street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL, verbose = FALSE) {
  
  # package all non-NULL address arguments into a named list
  combined_addr <- as.list(environment())
  combined_addr <- combined_addr[!names(combined_addr) %in% c('verbose')]
  combined_addr[sapply(combined_addr, is.null)] <- NULL # remove NULL items
  
  # trim all white space
  for (param in names(combined_addr)) combined_addr[param] <- lapply(combined_addr[param], trimws)
  
  if (verbose == TRUE) {
    display_named_list(combined_addr)
    #message(paste0('combined_addr class: ', class(combined_addr)))
  }
  
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
  crosswalk <- crosswalk[order(crosswalk['.id']), ]  # reorder
  
  return(list(unique = tibble::as_tibble(unique_addr[!names(unique_addr) %in% c('.uid')]), 
              crosswalk = tibble::as_tibble(crosswalk[!names(crosswalk) %in% addr_col_names])
  ))
}
#
# Function for unpackaging and RE-deduping addresses
# so that we can return them in the same order that they were passed
# this function assumes that the results are in the same order as package$unique
# @param unique_only if TRUE then only unique results are returned
# 
# @export
unpackage_addresses <- function(package, results, unique_only = FALSE, return_addresses = FALSE) {
  
  # Add addresses to results if we are returning them
  if (return_addresses == TRUE) results <- cbind(package$unique, results)
  
  # if there are no duplicates then just return the raw results
  if ((nrow(package$unique) == nrow(package$crosswalk)) | unique_only) return(results)
  
  # If there are duplicates then we need to use the crosswalk to return results properly
  id_colnames <- names(package$crosswalk)
  
  # on the off chance that the results dataset has the
  # id colnames in it then we remove them
  results <- results[!names(results) %in% id_colnames]
  
  # create unique id column in results 
  results[['.uid']] <- 1:nrow(results)
  
  # join crosswalk and results
  base <- merge(package$crosswalk, results, by = '.uid', all.x = TRUE, sort = FALSE)
  base <- base[order(base['.id']), ]  # reorder

  return(tibble::as_tibble(base[!names(base) %in% id_colnames]))
}