num_coords <- 5

set.seed(42)

lat_limits <- c(40.40857, 40.42585)
long_limits <- c(-3.72472, -3.66983)

random_lats <- runif(
  num_coords, 
  min = lat_limits[1], 
  max = lat_limits[2]
)

random_longs <- runif(
  num_coords, 
  min = long_limits[1], 
  max = long_limits[2]
)



url_base  <- 'https://nominatim.openstreetmap.org/reverse'


soup <- httr::GET(url = url_base, 
                  query = list(lat = random_lats[1], 
                               lon = random_longs[1],
                              namedetails = 1,
                              addressdetails = 1,
                              extratags = 1,
                               limit = 1,
                               format = 'json'
                                 ))

response <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))


# results_full <- cbind(response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))], 
#       tibble::as_tibble(response[['address']]), tibble::tibble(boundingbox = list(response$boundingbox))) 


results_full <- tidygeocoder:::extract_reverse_results('osm', response)


test <- dplyr::bind_cols(tibble::as_tibble(response[!(names(response) %in% c('display_name', 'boundingbox', 'address'))]), 
              tibble::as_tibble(response[['address']]), tibble::tibble(boundingbox = list(response$boundingbox)))


#f_address <- raw_results$display_name
#df_not_address <- raw_results[!(names(raw_results) %in% c('display_name', 'boundingbox'))]

#lapply(full_list, function(x) do.call(rbind, x))

#full_results <- dplyr::bind_cols(df_address, df_not_address)


#results <- tidygeocoder::extract_results(selected_method, raw_results)



