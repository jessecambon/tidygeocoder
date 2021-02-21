selected_method <- 'osm'

url_base  <- 'https://nominatim.openstreetmap.org/reverse'


soup <- httr::GET(url = url_base, 
                  query = list(lat = 6.455027, 
                               lon = 3.384082,
                               limit = 1,
                               format = 'json'
                                 ))

raw_results <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results <- tidygeocoder:::extract_reverse_results(selected_method, raw_results)


#f_address <- raw_results$display_name
#df_not_address <- raw_results[!(names(raw_results) %in% c('display_name', 'boundingbox'))]

#lapply(full_list, function(x) do.call(rbind, x))

#full_results <- dplyr::bind_cols(df_address, df_not_address)


#results <- tidygeocoder::extract_results(selected_method, raw_results)



