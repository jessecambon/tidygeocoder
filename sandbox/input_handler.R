

a <- tibble::tibble(city  = c('Lou','Ve','NY','Por','Bir','Bir','Lou', ''), 
            state = c('KY','NY','NY','ME','AL','AL','KY', ''))
b <- tibble::tibble(lat  = c(1,2,3,NA,1,2,3),
            long = c(1,2,3,NA,4,2,3))
c <- tibble::tibble(city = c('', ' '), state = c(' ', ''))
#####

### Select a test case
input_orig <- b
####################


input_colnames <- names(input_orig) # store column names

# Clean and deduplicate addresses. Remove all NA/missing addresses 
input_unique <- input_orig
# remove rows that are entirely blank or NA
input_unique <- input_unique[!apply(is.na(input_unique) | input_unique == "", 1, all), ]
# only keep unique columns and then create a unique identifier column
input_unique <- unique(input_unique)

# if there are 0 valid/nonblank addresses... return a tibble with 1 row for unique
# and a crosswalk the same length as input addresses
if (nrow(input_unique) == 0) {
  NA_list <- as.list(rep(NA, length(input_colnames)))
  names(NA_list) <- input_colnames
  return(list(unique = tibble::as_tibble(NA_list), 
              crosswalk = tibble::tibble(.id = 1:nrow(input_orig), .uid=1)))
}

# Create unique identifiers
input_unique[['.uid']] <- 1:nrow(input_unique)
# create id to record original address order
input_orig[['.id']] <- 1:nrow(input_orig)

# crosswalk
crosswalk <- merge(input_orig, input_unique, by = input_colnames, all.x = TRUE, sort = FALSE)
crosswalk <- crosswalk[order(crosswalk[['.id']]), ]  # reorder

# Return a named list containing two dataframes.
# unique contains all the unique addresses to pass to the geocoder service
# crosswalk contains only the .uid and .id columns to match the geocoder results
# back to the original input addresses (which may contain duplicates/blanks/etc)
final_results <- list(unique = tibble::as_tibble(input_unique[!names(input_unique) %in% c('.uid')]), 
            crosswalk = tibble::as_tibble(crosswalk[!names(crosswalk) %in% input_colnames]))

crosswalk
