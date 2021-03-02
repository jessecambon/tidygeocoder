selected_method <- "mapbox"

# https://docs.mapbox.com/api/search/geocoding/#reverse-geocoding

url_base <- tidygeocoder:::get_mapbox_url()

lat <- 40.4055517
lon <- -3.6802152

soup <-
  httr::GET(
    url = gsub(" ", "%20", paste0(url_base, lon, ",", lat, ".json")),
    query = list(
      limit = 1,
      access_token = tidygeocoder:::get_key(selected_method)
    )
  )

response <-
  jsonlite::fromJSON(httr::content(soup, as = "text", encoding = "UTF-8"))

results_min <-
  tidygeocoder:::extract_reverse_results(selected_method, response,
    full_results = FALSE
  )

results_full <-
  tidygeocoder:::extract_reverse_results(selected_method, response)

full_results_notflat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = FALSE
  )
full_results_flat <-
  tidygeocoder::extract_reverse_results(selected_method,
    response,
    full_results = TRUE,
    flatten = TRUE
  )
