# Functions for constructing the REST API Queries

### Style Guide
# http://adv-r.had.co.nz/Style.html

### API references
# IQ: https://locationiq.com/docs
# OSM: https://nominatim.org/release-docs/develop/api/Search/

### Usage Policies
# OSM: https://operations.osmfoundation.org/policies/nominatim/


### My Notes
# use names() function to extract names of named list

api_url_reference <- tibble::tribble(
  ~method,    ~name,         ~api_url,
  'osm',      "default",     "http://nominatim.openstreetmap.org/search",
  'iq',       'us',          "https://us1.locationiq.com/v1/search.php",
  'iq',       'europe',      "https://eu1.locationiq.com/v1/search.php",
  'geocodio', 'default',     "https://api.geocod.io/v1.5/geocode",
  'census',   'default',      "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress",
)

# Use this for storing essential parameters 
# Note format = JSON is required because our parsing code expects json content
api_parameter_reference <- tibble::tribble(
  ~method,  ~generic_name,  ~api_name,   ~default_value,   ~required,
  'osm',      'format',    'format',    'json',           TRUE,
  'osm',      'limit',     'limit',     '1',              FALSE,
  'osm',      'address',   'q',         NA,               FALSE,
  'iq',       'address',   'q',         NA,               FALSE,
  'iq',       'limit',     'limit',     '1',              FALSE,
  'iq',       'api_key',   'key',       NA,               TRUE,
  'iq',       'format',    'format',    'json',           TRUE,
  'geocodio', 'api_key',   'api_key',   NA,               TRUE,
  'geocodio', 'limit',     'limit',     '1',              FALSE,
  'geocodio', 'format',     NA,         NA,               FALSE,
  'geocodio', 'address',    'q',        NA,               FALSE,
  'census',   'address',   'address',   NA,               FALSE,
  'census',   'format',    'format',    'json',           TRUE,
  'census',   'limit',     NA,          NA,               FALSE,
  'census',    NA,         'benchmark', '4',              TRUE,
)

usethis::use_data(api_parameter_reference, overwrite = TRUE)
usethis::use_data(api_url_reference, overwrite = TRUE)
