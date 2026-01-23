addr <- "197 Trần Phú, Phường 4, Quận 5, TP. Hồ Chí Minh"

# ==== check api endpoint ====
url <- tidygeocoder:::get_vietmap_url()
url
tidygeocoder:::get_vietmap_url(reverse = TRUE)

# ==== check key getter ====
tidygeocoder::api_key_reference
key <- tidygeocoder:::get_key("vietmap")

# ==== test geocoding code ====
api_query_params <- list(
  text = addr,
  apikey = key,
  display_type=6
)

# First get result from Search API
# uncomment to test querying
# response <- httr::GET(url, query = api_query_params)
content <- httr::content(response, as = "text", encoding = "UTF-8")
raw_results <- jsonlite::fromJSON(content)

# Then get reference id of the first result
ref_id <- raw_results[1,"ref_id"]

# Finally, get geocode from Place API
# uncomment to test querying
# geo_response <- httr::GET("https://maps.vietmap.vn/api/place/v4", 
#                       query = list(
#                         apikey = key,
#                         refid = ref_id
#                       ))
geo_content <- httr::content(geo_response, as = "text", encoding = "UTF-8")


# ==== Test high level code ====
# uncomment to rerun query
# test_res <- tidygeocoder::query_api(url, 
#                                     api_query_params, method="vietmap")
extract_results("vietmap", jsonlite::fromJSON(test_res$content))
extract_results("vietmap", jsonlite::fromJSON(test_res$content), full_results = FALSE)


# Try geo() function
# uncomment to test querying
# test_out <- tidygeocoder::geo(
#   address = addr,
#   method = "vietmap",
#   lat = latitude,
#   long = longitude,
#   api_options = list(
#     "vietmap_display_type" = 6
#   )
# )

# Try geocode with invalid address
# tidygeocoder::geo(
#     address = "sgherhgewsh",
#     method = "vietmap",
#     lat = latitude,
#     long = longitude,
#     api_options = list(
#       "vietmap_display_type" = 6
#     )
#   )


test_out

