### Dataframes for storing API parameters and urls

## Note: generic_name = 'address' is for one-line addresses
## If generic_name == NA then that means the parameter is specific to a given API/method

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

  ######################### OpenCage #################################
  # OpenCage returns json by default (defined by endpoint in URL) 
  # OpenCage requires an api_key
  # OpenCage does not support structured queries (street, city, state, etc.)
  
  'opencage', 'api_key',    'key',         NA,                  TRUE,
  'opencage', 'address',    'q',           NA,                  TRUE,  
  'opencage', 'limit',      'limit',       '1',                 FALSE,
  
  ########################### Mapbox #################################
  # Mapbox returns json by default (defined by endpoint in URL) 
  # Mapbox requires an api_key
  
  'mapbox', 'api_key',    'access_token',  NA,                  TRUE,
  'mapbox', 'address',    'search_text',   NA,                  TRUE,  
  'mapbox', 'limit',      'limit',        '1',                  FALSE,
  
  ###########################  HERE  #################################
  # HERE returns json by default
  # HERE requires an api_key
  
  'here', 'api_key',    'apiKey',          NA,                  TRUE,
  'here', 'address',    'q',               NA,                  TRUE,  
  'here', 'limit',      'limit',           '1',                 FALSE,
  

  ########################### TomTom #################################
  # TomTom returns json (defined by endpoint in URL) 
  # TomTom requires an api_key
  
  'tomtom', 'api_key',    'key',          NA,                  TRUE,
  'tomtom', 'address',    'query',        NA,                  TRUE,  
  'tomtom', 'limit',      'limit',        '1',                 FALSE,
  
  ########################### MapQuest #################################
  # MapQuest returns json by default
  # MapQuest requires an api_key
  
  'mapquest', 'api_key',    'key',           NA,                  TRUE,
  'mapquest', 'address',    'location',      NA,                  TRUE,  
  'mapquest', 'limit',      'maxResults',    '1',                 FALSE,
  
  ###########################    Bing    #################################
  # Bing returns json by default
  # Bing requires an api_key
  
  'bing',      'api_key',    'key',           NA,                  TRUE,
  'bing',      'address',    'q',             NA,                  TRUE,  
  'bing',      'limit',      'maxResults',    '1',                 FALSE,
  
  ########################### ArcGis #################################
  # ArcGis may not require an api key
  
  'arcgis', 'address',    'SingleLine',   NA,                  FALSE,  
  'arcgis', 'limit',      'maxLocations', '1',                 FALSE,
  'arcgis', 'format',     'f',            'json',              TRUE,
)

usethis::use_data(api_parameter_reference, overwrite = TRUE)
