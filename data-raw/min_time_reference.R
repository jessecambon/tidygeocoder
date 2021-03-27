### Minimum number of seconds required per query for each service
### to comply with usage guidelines.


# stores the minimum number of seconds that must elapse for each query
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
usethis::use_data(min_time_reference, overwrite = TRUE)

