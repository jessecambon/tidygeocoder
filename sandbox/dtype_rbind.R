# Objective: address https://github.com/jessecambon/tidygeocoder/issues/160

library(dplyr, warn.conflicts = FALSE)
#library(vctrs)
library(skimr)

# create an mtcars dataset with a character mpg column
mtcars_char <- mtcars %>% mutate(mpg = as.character(mpg))

# put the dataframes in a list
mtcars_list <- list(mtcars, mtcars_char)

# create a dataframe with one row per dataframe in mtcars_list
# and each column shows the type
types <- bind_rows(lapply(mtcars_list, function(x) bind_rows(sapply(x, class))))

# find column names which have both numeric and character content
identified_cols <- sapply(types, function(x) ("character" %in% x) & ("numeric" %in% x))

# remove FALSE values to extract column name(s) which require attention
identified_cols <- names(identified_cols[identified_cols == TRUE])

# convert identified_cols to character for all dataframes in the list if necessary

# convert a named column to character
convert_col_to_char <- function(df, colname) {
  df[colname] <- as.character(df[[colname]])
  return(df)
}

# fconvert the columns specified in the character vector 'column_names'
# to character
convert_all_relevant_cols <- function(df, column_names) {
  for (colname in column_names) {
    if (colname %in% colnames(df)) {
      df <- convert_col_to_char(df, colname)
    }
  }
  return(df)
}

# convert list of dataframes into a list of dataframes with datatypes that are
# compatible for bind_rows() 
mtcars_converted <- lapply(mtcars_list, convert_all_relevant_cols, column_names = identified_cols)

# bind list of dataframes into single dataframe
mtcars_combined <- tibble(bind_rows(mtcars_converted))