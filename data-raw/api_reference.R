### Dataframes for storing API parameters and urls

### Style Guide
# http://adv-r.had.co.nz/Style.html

### API references
# IQ: https://locationiq.com/docs
# OSM: https://nominatim.org/release-docs/develop/api/Search/
# Geocodio: https://www.geocod.io/docs/#single-address
# Census: https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html
# Google: https://developers.google.com/maps/documentation/geocoding/overview

### Usage Policies
# OSM: https://operations.osmfoundation.org/policies/nominatim/
# Google: https://developers.google.com/maps/documentation/geocoding/usage-and-billing

## Note: generic_name = 'address' is for one-line addresses
## If generic_name == NA then that means the parameter is specific to a given API/method
## If api_name == NA then that means the parameter isn't available for the given API/method

# Use this for storing essential parameters 
# Note that the format = JSON parameter is required because our parsing code expects json content
api_parameter_reference <- tibble::tribble(
  ~method,  ~generic_name,   ~api_name,    ~default_value,      ~required,
  
  #########################  Census #################################
  # vintage is only required when return = 'geographies'
  # there is no limit argument
  
  'census',   'format',     'format',      'json',              TRUE,
  'census',    NA,          'benchmark',   'Public_AR_Current', TRUE,
  'census',    NA,          'vintage',     'Current_Current',   TRUE,
  'census',   'address',    'address',     NA,                  FALSE,
  'census',   'street',     'street',      NA,                  FALSE,
  'census',   'city',       'city',        NA,                  FALSE,
  'census',   'state',      'state',       NA,                  FALSE,
  'census',   'postalcode', 'zip',         NA,                  FALSE,

  ###########################  OSM ###################################
  
  'osm',      'format',     'format',      'json',              TRUE,
  'osm',      'limit',      'limit',       '1',                 FALSE,
  'osm',      'address',    'q',           NA,                  FALSE,
  'osm',      'street',     'street',      NA,                  FALSE,
  'osm',      'city',       'city',        NA,                  FALSE,
  'osm',      'county',     'county',      NA,                  FALSE,
  'osm',      'state',      'state',       NA,                  FALSE,
  'osm',      'postalcode', 'postalcode',  NA,                  FALSE,
  'osm',      'country',    'country',     NA,                  FALSE,
  
  ###########################  IQ  ###################################
  # iq shares the same parameters as OSM but requires an api_key 
  
  'iq',       'api_key',    'key',         NA,                  TRUE,
  'iq',       'format',     'format',      'json',              TRUE,
  'iq',       'limit',      'limit',       '1',                 FALSE,
  'iq',       'address',    'q',           NA,                  FALSE,
  'iq',       'street',     'street',      NA,                  FALSE,
  'iq',       'city',       'city',        NA,                  FALSE,
  'iq',       'county',     'county',      NA,                  FALSE,
  'iq',       'state',      'state',       NA,                  FALSE,
  'iq',       'postalcode', 'postalcode',  NA,                  FALSE,
  'iq',       'country',    'country',     NA,                  FALSE,
  
  #########################  Geocodio ################################
  # geocodio returns json by default and has no 'format' or 'county' arguments
  
  'geocodio', 'api_key',    'api_key',     NA,                  TRUE,
  'geocodio', 'limit',      'limit',       '1',                 FALSE,
  'geocodio', 'address',    'q',           NA,                  FALSE,
  'geocodio', 'street',     'street',      NA,                  FALSE,
  'geocodio', 'city',       'city',        NA,                  FALSE,
  'geocodio', 'state',      'state',       NA,                  FALSE,
  'geocodio', 'postalcode', 'postal_code', NA,                  FALSE,
  'geocodio', 'country',    'country',     NA,                  FALSE,

  #########################  Google Geocoding ########################
  # Google returns json by default and has no 'format' argument
  # requires an api_key
  # no limit argument in this implementation

  'google', 'api_key',      'key',         NA,                  TRUE,
  'google', 'address',      'address',     NA,                  TRUE,
)

usethis::use_data(api_parameter_reference, overwrite = TRUE)
