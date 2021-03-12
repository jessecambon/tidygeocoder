selected_method <- 'geocodio'

# https://www.geocod.io/docs/#reverse-geocoding
url_base  <- 'https://api.geocod.io/v1.6/reverse'

# white house, toronto, junk lat/lng
lat <- c(38.89586, 43.6534817, 600)
long <- c(-77.0307713, -79.3839347, -214)

paste0(as.character(lat), ',', as.character(long))

raw_response <- tidygeocoder:::query_api(url_base, mode = 'list',
         address_list = paste0(as.character(lat), ',', as.character(long)),
         query_parameters = list(limit = 1,
                                api_key = tidygeocoder:::get_key(selected_method)))

response <- jsonlite::fromJSON(raw_response, flatten = TRUE)

result_list <- response$results$response.results

# if no results are returned for a given address then there is a 0 row dataframe in this
# list and we need to replace it with a 1 row NA dataframe to preserve the number of rows
result_list_filled <- lapply(result_list, tidygeocoder:::filler_df, c('formatted_address'))

# combine list of dataframes into a single tibble. Column names may differ between the dataframes
results <- dplyr::bind_rows(result_list_filled)

final_results <- cbind(results[c('formatted_address')], results[!names(results) %in% c('formatted_address')])

