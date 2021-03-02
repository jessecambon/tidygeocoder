selected_method <- 'mapbox'

addr <- 'Acueducto de Segovia, Spain'
url_base  <- tidygeocoder:::get_mapbox_url()

library(httr)
library(jsonlite)
library(dplyr)


soup <-
  httr::GET(
    url = gsub(" ", "%20", paste0(url_base, addr, ".json")),
    query = list(
      limit = 1,
      access_token = tidygeocoder:::get_key(selected_method)
    )
  )

raw_results <-
  jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"))

results_minimal <-
  tidygeocoder::extract_results(selected_method, raw_results, full_results = FALSE)

results <-
  tidygeocoder::extract_results(selected_method, raw_results)

full_results_notflat <-
  tidygeocoder::extract_results(selected_method,
                                raw_results,
                                full_results = TRUE,
                                flatten = FALSE)
full_results_flat <-
  tidygeocoder::extract_results(selected_method,
                                raw_results,
                                full_results = TRUE,
                                flatten = TRUE)
