### Dataframes for storing API parameters and urls

### Style Guide
# http://adv-r.had.co.nz/Style.html

### API references
# IQ: https://locationiq.com/docs
# OSM: https://nominatim.org/release-docs/develop/api/Search/
# Geocodio: https://www.geocod.io/docs/#single-address
# Census: https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html

### Usage Policies
# OSM: https://operations.osmfoundation.org/policies/nominatim/

## Note: generic_name = 'address' is for one-line addresses
## If generic_name == NA then that means the parameter is specific to a given API/method
## If api_name == NA then that means the parameter isn't available for the given API/method

# Use this for storing essential parameters 
# Note that the format = JSON parameter is required because our parsing code expects json content
api_parameter_reference <- tibble::tribble(
  ~method,  ~generic_name,   ~api_name,    ~default_value,      ~required,
  
  # Note that vintage is only required when return = 'geographies'
  'census',   'format',     'format',      'json',              TRUE,
  'census',   'limit',      NA,            NA,                  FALSE,
  'census',    NA,          'benchmark',   'Public_AR_Current', TRUE,
  'census',    NA,          'vintage',     'Current_Current',   TRUE,
  'census',   'address',    'address',     NA,                  FALSE,
  'census',   'street',     'street',      NA,                  FALSE,
  'census',   'city',       'city',        NA,                  FALSE,
  'census',   'county',     NA,            NA,                  FALSE,
  'census',   'state',      'state',       NA,                  FALSE,
  'census',   'postalcode', 'zip',         NA,                  FALSE,
  'census',   'country',    NA,            NA,                  FALSE,
  
  'osm',      'format',     'format',      'json',            TRUE,
  'osm',      'limit',      'limit',       '1',               FALSE,
  'osm',      'address',    'q',           NA,                FALSE,
  'osm',      'street',     'street',      NA,                FALSE,
  'osm',      'city',       'city',        NA,                FALSE,
  'osm',      'county',     'county',      NA,                FALSE,
  'osm',      'state',      'state',       NA,                FALSE,
  'osm',      'postalcode', 'postalcode',  NA,                FALSE,
  'osm',      'country',    'country',     NA,                FALSE,
  
  'iq',       'api_key',    'key',         NA,                TRUE,
  'iq',       'limit',      'limit',       '1',               FALSE,
  'iq',       'format',     'format',      'json',            TRUE,
  'iq',       'address',    'q',           NA,                FALSE,
  'iq',       'street',     'street',      NA,                FALSE,
  'iq',       'city',       'city',        NA,                FALSE,
  'iq',       'county',     'county',      NA,                FALSE,
  'iq',       'state',      'state',       NA,                FALSE,
  'iq',       'postalcode', 'postalcode',  NA,                FALSE,
  'iq',       'country',    'country',     NA,                FALSE,
  
  'geocodio', 'api_key',    'api_key',     NA,                TRUE,
  'geocodio', 'limit',      'limit',       '1',               FALSE,
  'geocodio', 'format',     NA,            NA,                FALSE,
  'geocodio', 'address',    'q',           NA,                FALSE,
  'geocodio', 'street',     'street',      NA,                FALSE,
  'geocodio', 'city',       'city',        NA,                FALSE,
  'geocodio', 'county',     NA,            NA,                FALSE,
  'geocodio', 'state',      'state',       NA,                FALSE,
  'geocodio', 'postalcode', 'postal_code', NA,                FALSE,
  'geocodio', 'country',    'country',     NA,                FALSE,
)

# Note - see function in query_factory.R for census API URL
api_url_reference <- tibble::tribble(
  ~method,    ~name,                 ~api_url,
  'osm',      "default",             "http://nominatim.openstreetmap.org/search",
  'iq',       'us',                  "https://us1.locationiq.com/v1/search.php",
  'iq',       'europe',              "https://eu1.locationiq.com/v1/search.php",
  'geocodio', 'v1.5',                "https://api.geocod.io/v1.5/geocode"
)

usethis::use_data(api_parameter_reference, overwrite = TRUE)
usethis::use_data(api_url_reference, overwrite = TRUE)
