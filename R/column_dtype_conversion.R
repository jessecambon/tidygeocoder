## Functions for converting the datatype of columns so they can be 
## combined with dplyr::bind_rows() 
## Addresses this issue: https://github.com/jessecambon/tidygeocoder/issues/160
## See dtype_rbind_function.R and dtype_rbind.R in the sandbox folder 


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
# columns have compatible data types for bind_rows()
# (makes conversion from numeric to character as necessary)
make_coltypes_compatible <- function(df_list) {
  # create a dataframe with one row per dataframe in df_list
  # and each column shows the type
  types <- dplyr::bind_rows(lapply(df_list, function(x) dplyr::bind_rows(sapply(x, class))))
  
  # find column names which have both numeric and character content
  identified_cols <- sapply(types, function(x) ("character" %in% x) & (("numeric" %in% x) | "integer" %in% x))
  
  # extract column name(s) which require conversion
  identified_cols <- names(identified_cols[identified_cols == TRUE])
  
  # convert list of dataframes into a list of dataframes with datatypes that are
  # compatible for bind_rows() 
  df_list_converted <- lapply(df_list, convert_all_relevant_cols, column_names = identified_cols)
  return(df_list_converted)
}
