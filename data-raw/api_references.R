### Maximum number of addresses/coordinates allowed in a batch query
### for each service
batch_limit_reference <- tibble::tribble(
  ~method, ~batch_limit,
  "census", 1e4,
  "geocodio", 1e4,
  "tomtom", 1e4,
  "here", 1e6,
  "mapquest", 100,
  "bing", 50,
)

### Minimum number of seconds required per query for each service
### to comply with usage guidelines.
# based on the usage limit of the service (free tier if there are multiple plans available)
# Stored value is SECONDS PER QUERY

min_time_reference <- tibble::tribble(
  ~method, ~min_time, ~description,
  "osm", 1, "1 query per second",
  "geocodio", 60 / 1000, "1000 queries per minute (free tier)",
  "iq", 1, "60 queries per minute (free tier)",
  "google", 1 / 50, "50 queries per second",
  "opencage", 1, "1 query per second (free tier)",
  "mapbox", 60 / 600, "600 queries per minute (free tier)",
  "tomtom", 1 / 5, "5 queries per second (free tier)",
  "here", 1 / 5, "5 queries per second (free tier)",
  "geoapify", 1 / 5, "5 queries per second (free tier)",
)


# The environmental variable name where an API key should be stored
# used by the get_key() function
api_key_reference <- tibble::tribble(
  ~method,     ~env_var,
  "geocodio",  "GEOCODIO_API_KEY",
  "iq",        "LOCATIONIQ_API_KEY",
  "google",    "GOOGLEGEOCODE_API_KEY",
  "opencage",  "OPENCAGE_KEY",
  "mapbox",    "MAPBOX_API_KEY",
  "here",      "HERE_API_KEY",
  "tomtom",    "TOMTOM_API_KEY",
  "mapquest",  "MAPQUEST_API_KEY",
  "bing",      "BINGMAPS_API_KEY",
  "geoapify",  "GEOAPIFY_KEY",
)


api_info_reference <- tibble::tribble(
  ~method, ~method_display_name, ~site_url, ~api_documentation_url, ~api_usage_policy_url,
  "osm", "Nominatim", "https://nominatim.org", "https://nominatim.org/release-docs/develop/api/Search/", "https://operations.osmfoundation.org/policies/nominatim/",
  "census", "US Census", "https://geocoding.geo.census.gov/", "https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html", "https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html",
  "arcgis", "ArcGIS", "https://developers.arcgis.com/rest/geocode/api-reference/overview-world-geocoding-service.htm", "https://developers.arcgis.com/rest/geocode/api-reference/overview-world-geocoding-service.htm", "https://developers.arcgis.com/pricing/",
  "geocodio", "Geocodio", "https://www.geocod.io/", "https://www.geocod.io/docs/", "https://www.geocod.io/pricing/",
  "iq", "Location IQ", "https://locationiq.com/", "https://docs.locationiq.com/docs/introduction", "https://locationiq.com/pricing",
  "google", "Google", "https://developers.google.com/maps/documentation/geocoding/overview", "https://developers.google.com/maps/documentation/geocoding/overview", "https://developers.google.com/maps/documentation/geocoding/usage-and-billing",
  "opencage", "OpenCage", "https://opencagedata.com", "https://opencagedata.com/api", "https://opencagedata.com/pricing",
  "mapbox", "Mapbox", "https://docs.mapbox.com/api/search/", "https://docs.mapbox.com/api/search/geocoding/", "https://www.mapbox.com/pricing",
  "here", "HERE", "https://www.here.com/platform/geocoding", "https://www.here.com/docs/bundle/geocoding-and-search-api-developer-guide/page/README.html", "https://www.here.com/get-started/pricing",
  "tomtom", "TomTom", "https://developer.tomtom.com/search-api/search-api-documentation", "https://developer.tomtom.com/search-api/search-api-documentation", "https://developer.tomtom.com/store/maps-api",
  "mapquest", "MapQuest", "https://developer.mapquest.com/documentation/geocoding-api/", "https://developer.mapquest.com/documentation/geocoding-api/", "https://developer.mapquest.com/plans/",
  "bing", "Bing", "https://learn.microsoft.com/en-us/bingmaps/rest-services/locations/", "https://learn.microsoft.com/en-us/bingmaps/rest-services/locations/", "https://learn.microsoft.com/en-us/bingmaps/spatial-data-services/geocode-and-data-source-limits",
  "geoapify", "Geoapify", "https://www.geoapify.com/geocoding-api", "https://apidocs.geoapify.com/docs/geocoding/api/", "https://www.geoapify.com/term-and-conditions",
)

usethis::use_data(batch_limit_reference, overwrite = TRUE)
usethis::use_data(min_time_reference, overwrite = TRUE)
usethis::use_data(api_key_reference, overwrite = TRUE)
usethis::use_data(api_info_reference, overwrite = TRUE)
