# Load objects from breakpoint before error : https://github.com/jessecambon/tidygeocoder/issues/63
# breakpoint was inserted in address_handling.R

package <- readRDS('package.RDS')
results <- readRDS('results.RDS')

# replicate error in address_handling.R
results[['.uid']] <- 1:nrow(results)
#results <- as.data.frame(results)

## Works if all.x is set to FALSE (error on all.x = TRUE)
#base <- merge(package$crosswalk, results, by = '.uid', all.x = TRUE, sort = FALSE)
base <- dplyr::left_join(package$crosswalk, results, by = '.uid')
