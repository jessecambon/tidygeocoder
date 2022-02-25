# Objective: address https://github.com/jessecambon/tidygeocoder/issues/160

library(dplyr, warn.conflicts = FALSE)
library(skimr)

## PREPARE DATAFRAME LIST ---------------------------------------------------------

# create an mtcars dataset with a character mpg column
mtcars_char <- mtcars %>% mutate(mpg = as.character(mpg))
# put the dataframes in a list
mtcars_list <- list(mtcars, mtcars_char)

## AUXILLARY FUNCTION SPECIFICATION ----------------------------------------------

# convert a single named column ('colname') to character for an input dataframe 'df' 
convert_col_to_char <- function(df, colname) {
  df[colname] <- as.character(df[[colname]])
  return(df)
}

# convert the columns specified in the character vector 'column_names'
# to character
convert_all_relevant_cols <- function(df, column_names) {
  for (colname in column_names) {
    if (colname %in% colnames(df)) {
      df <- convert_col_to_char(df, colname)
    }
  }
  return(df)
}

## PRIMARY FUNCTION SPECIFICATION --------------------------------------------------------------

# iterate through a list of dataframes and ensure
# columns have compatible data types for dplyr::bind_rows()
# (makes conversion from numeric to character as necessary)
make_coltypes_compatible <- function(df_list) {
  # create a dataframe with one row per dataframe in df_list
  # and each column shows the type
  types <- dplyr::bind_rows(lapply(df_list, function(x) dplyr::bind_rows(sapply(x, class))))
  
  # find column names which have both numeric and character content
  identified_cols <- sapply(types, function(x) ("character" %in% x) & ("numeric" %in% x))
  
  # remove FALSE values to extract column name(s) which require attention
  identified_cols <- names(identified_cols[identified_cols == TRUE])
  
  # convert list of dataframes into a list of dataframes with datatypes that are
  # compatible for dplyr::bind_rows() 
  df_list_converted <- lapply(df_list, convert_all_relevant_cols, column_names = identified_cols)
  return(df_list_converted)
}

## EXECUTE AND TEST PRIMARY FUNCTION -----------------------------------------------------------

# bind list of dataframes into single dataframe
mtcars_combined <- tibble::tibble(dplyr::bind_rows(make_coltypes_compatible(mtcars_list)))

sapply(mtcars_combined, class)
class(mtcars_combined)