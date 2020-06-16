#### Ideas 
# Rename address column to something standard just to make sure it doesn't
# overlap with any of our functions. We can rename it back to what it was
# right before returning the data back to the user


library(tibble)
address_colname <- 'addr'
input_df <- rbind(sample_addresses,sample_addresses)

## Create id column which we will use for making sure our joined
## results are in the same order as original dataset
orig_addresses <- input_df[address_colname]
orig_addresses['.id'] <-  1:nrow(orig_addresses)

## Deduplicate addresses
unique_addresses <- unique(orig_addresses[address_colname])
unique_addresses['.uid'] <- 1:nrow(unique_addresses)  # dummy data instead of coordinates/etc
#colnames(unique_addresses) <- c(address_colname, 'data')

## Map duplicate address .id to .uid (unique id)
## We can now use the .uid vector to place our geocoded addresses onto the input dataset
crosswalk = merge(orig_addresses, unique_addresses, by = address_colname, all.x = TRUE, sort = FALSE)
crosswalk <- crosswalk[order(crosswalk['.id']),] 

# Fake some coordinate data
coords <- unique_addresses
coords$d <- runif(nrow(coords),-100,100)
coords$e <- runif(nrow(coords),-100,100)


combi <- coords %>% 
  dplyr::left_join(crosswalk, by = '.uid') %>%
  arrange(.id)
  
#### Package up our addresses into a named list
pack <- list()
pack$crosswalk <- crosswalk
pack$unique_addresses <- unique_addresses



# Merged, reorder by our 'id' column, and drop our id column (by position in case there is another 'id' column)
# merged <- merge(dup_map, unique_addresses, by = address_colname, all.x = TRUE, sort = FALSE)
# merged <- merged[order(merged[,2]),]      # reorder
# merged <- subset(merged, select = -c(2))  # drop .id column by position
# 
# final_df <- as_tibble(merged)