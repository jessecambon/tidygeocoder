# trims all whitespace from all columns
# converts any pure whitespace values to NA
trim_df <- function(df) {
  m <- as.matrix(df) 
  m[is.na(m)] <- '' # convert all NAs to blanks
  m <- apply(m, 2, trimws) # trim all whitespace
  m[m == ''] <- NA # convert all blanks to NAs
  
  # convert to dataframe and return
  trimmed_df <- tibble::as_tibble(m)
  names(trimmed_df) <- names(df)
  return(trimmed_df)
}

# Function for packaging and deduping addresses that are passed to the geo function
# package addresses
# @export
package_addresses <- function(address = NULL, 
  street = NULL, city = NULL, county = NULL, state = NULL, postalcode = NULL, country = NULL) {
  
  # package all non-NULL address arguments into a named list
  combined_addr <- as.list(environment())
  combined_addr[sapply(combined_addr, is.null)] <- NULL # remove NULL items
  arg_names <- names(combined_addr)
  
  ## QA check the address inputs 
  if (('address' %in% arg_names) & (length(arg_names) > 1)) {
    stop("Do not use other address component parameters with the single line 'address' parameter")
  }
  address_component_lengths <- lengths(combined_addr)
  if (max(address_component_lengths) != min(address_component_lengths)) {
    stop('Address components must be equal in length')
  }
  
  # Turn address inputs into a dataframe
  addr_orig <- tibble::as_tibble(combined_addr)
  addr_colnames <- names(addr_orig) # store column names
  
  # Trim whitespace and convert all purely whitespace values to NA
  addr_orig <- trim_df(addr_orig)
  
  ### Clean and deduplicate addresses. Remove all NA/missing addresses 
  unique_addr <- addr_orig
  # remove rows that are entirely blank or NA
  unique_addr <- unique_addr[!apply(is.na(unique_addr) | unique_addr == "", 1, all), ]
  # only keep unique columns and then create a unique identifier column
  unique_addr <- unique(unique_addr)
  
  # if there are 0 valid/nonblank addresses then 0 rows for unique tibble
  # and return a tibble of the same number of rows as the input for crosswalk
  if (nrow(unique_addr) == 0) return(list(unique=tibble::tibble(), crosswalk = tibble::tibble(.id = 1:nrow(addr_orig))))
  
  ## Create unique identifiers
  unique_addr[['.uid']] <- 1:nrow(unique_addr)
  # create id to record original address order
  addr_orig[['.id']] <- 1:nrow(addr_orig)
  
  # print('addr_orig:')
  # print(addr_orig)
  # print('unique_addr:')
  # print(unique_addr)
  
  # crosswalk
  crosswalk <- merge(addr_orig, unique_addr, by = addr_colnames, all.x = TRUE, sort = FALSE)
  crosswalk <- crosswalk[order(crosswalk['.id']), ]  # reorder
  
  # Return a named list containing two dataframes.
  # unique contains all the unique addresses to pass to the geocoder service
  # crosswalk contains only the .uid and .id columns to match the geocoder results
  # back to the original input addresses (which may contain duplicates/blanks/etc)
  return(list(unique = tibble::as_tibble(unique_addr[!names(unique_addr) %in% c('.uid')]), 
              crosswalk = tibble::as_tibble(crosswalk[!names(crosswalk) %in% addr_colnames])
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