selected_method <- 'osm'

url_base  <- 'https://nominatim.openstreetmap.org/reverse'


soup <- httr::GET(url = url_base, 
                  query = list(lat = 6.455027, 
                               lon = 3.384082,
                               limit = 1,
                               format = 'json'
                                 ))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

df_address <- tibble::as_tibble(raw_results$address)
df_not_address <- lapply(raw_results[!(names(raw_results) %in% 'address')], function(x) do.call(rbind, x))

lapply(full_list, function(x) do.call(rbind, x))

full_results <- dplyr::bind_cols(df_address, df_not_address)


#results <- tidygeocoder::extract_results(selected_method, raw_results)



