### Maximum number of addresses/coordinates allowed in a batch query
### for each service
batch_limit_reference <- tibble::tribble(
  ~method,   ~batch_limit, 
  "census",    1e4,
  "geocodio",  1e4,
  "tomtom",    1e4,       
  "here",      1e6,    
  "mapquest",  100,    
  "bing",      50,
)
  
### Minimum number of seconds required per query for each service
### to comply with usage guidelines.
# based on the usage limit of the service (free tier if there are multiple plans available)
# Stored value is SECONDS PER QUERY

min_time_reference <- tibble::tribble(
  ~method,     ~min_time,    ~description,
  "osm",       1,            "1 query per second",        
  "geocodio",  60/1000,      "1000 queries per minute (free tier)",
  "iq",        1/2,          "2 queries per second (free tier)",
  "google",    1/50,         "50 queries per second",
  "opencage",  1,            "1 query/second",
  "mapbox",    60/600,       "600 queries per minute (free tier)",
  "tomtom",    1/5,          "5 queries per second (free tier)",
  "here",      1/5,          "5 queries per second (free tier)",
)


# The environmental variable name where an API key should be stored
# used by the get_key() function 
api_key_reference <- tibble::tribble(
  ~method,     ~env_var,
  'geocodio',  "GEOCODIO_API_KEY",
  'iq',        "LOCATIONIQ_API_KEY",
  'google',    "GOOGLEGEOCODE_API_KEY",
  'opencage',  "OPENCAGE_KEY",
  'mapbox',    "MAPBOX_API_KEY",
  'here',      "HERE_API_KEY",
  'tomtom',    "TOMTOM_API_KEY",
  'mapquest',  "MAPQUEST_API_KEY",
  'bing',      "BINGMAPS_API_KEY",
)

usethis::use_data(batch_limit_reference, overwrite = TRUE)
usethis::use_data(min_time_reference, overwrite = TRUE)
usethis::use_data(api_key_reference, overwrite = TRUE)