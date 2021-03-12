library(tidyverse)


coords <- tibble(lat = 1, long = 2)
results <- tibble(address = 'yadda', street = 'xyz', lat = 1, long = 2)


# if there is a column conflict then rename the columns in the 
# second dataset. Then merge both datasets with dplyr::bind_cols
# this avoids columns in the first dataset from being renamed
# .suffix is appending to columns in the case of conflict
# df2 is returned
rename_and_bind_cols <- function(df1, df2, suffix = 'results') {
  
  # what column names are in common between the two datasets
  names_in_common <- intersect(names(df1), names(df2))
  
  # iterate through column names in common (if any) and rename
  if (length(names_in_common) != 0) {
    for (name in names_in_common) {
      names(df2)[which(names(df2) == name)] <- paste0(name, '.', suffix)
    }
  }
  return(dplyr::bind_cols(df1, df2))
}


combi <- rename_and_bind_cols(coords, results)