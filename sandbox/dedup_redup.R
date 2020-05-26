library(tibble)

dup_addresses <- rbind(sample_addresses,sample_addresses)

## Create id column which we will use for making sure our joined
## results are in the same order as original dataset
dup_map <- data.frame(addr = dup_addresses$addr, .id = 1:nrow(dup_addresses))

# Deduplicate addresses
unique_addresses <- as_tibble(unique(dup_addresses[['addr']])) 
unique_addresses[['data']] <- 11:19  # dummy data instead of coordinates/etc
colnames(unique_addresses) <- c('addr', 'data')

# Merged, reorder by our 'id' column, and drop our id column (by position in case there is another 'id' column)
merged <- merge(dup_map, unique_addresses, by = 'addr', all.x = TRUE, sort = FALSE)
merged <- merged[order(merged[,2]),]      # reorder
merged <- subset(merged, select = -c(2))  # drop .id column by position

final_df <- as_tibble(merged)