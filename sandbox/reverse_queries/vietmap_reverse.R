lat <- 10.759223
lgn <- 106.675903

# Get API endpoint for reverse geocoding
rev_url <- tidygeocoder:::get_vietmap_url(reverse = TRUE)

# ==== Test function to generate params for reverse geocoding ===
rev_param <- list(
  "apikey" = tidygeocoder:::get_key("vietmap"),
  display_type=6
)
rev_param <- tidygeocoder:::get_coord_parameters(
  rev_param,
  method="vietmap", 
  long = lgn, lat = lat
)

# uncomment to test querying
# response <- tidygeocoder::query_api(rev_url, rev_param, method="vietmap")

tidygeocoder::extract_reverse_results("vietmap", jsonlite::fromJSON(response$content))
tidygeocoder::extract_reverse_results("vietmap", jsonlite::fromJSON(response$content), full_results=FALSE)


# try high level function
# uncomment to test querying
# test_out <- tidygeocoder::reverse_geo(lat = lat, long = lgn, method = "vietmap")
test_out

# try out of scope geocode
# tidygeocoder::reverse_geo(lat = 38.89770, long = -77.03655, method = "vietmap")

