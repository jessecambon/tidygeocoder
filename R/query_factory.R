
library(tibble)


api_url_reference <- tribble(
  ~method,    ~name,         ~api_url,
  'osm',      "default",    "http://nominatim.openstreetmap.org/search",
  'iq',       'us',         "https://us1.locationiq.com/v1/search.php",
  'iq',       'europe',      "insert/europe/here",
  'geocodio', 'default',     "https://api.geocod.io/v1.5/geocode",
  'census',   'default',      "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress",
)


# Use this for storing essential parameters 
api_parameter_reference <- tribble(
    ~method,  ~generic_name,  ~api_name,   ~default_value,   ~required,
    'osm',      'format',    'format',    'json',           TRUE,
    'osm',      'limit',     'limit',     '1',              FALSE,
    'osm',      'address',   'q',         NA,               TRUE,
    'iq',       'address',   'q',         NA,               TRUE,
    'iq',       'limit',     'limit',     '1',              FALSE,
    'iq',       'api_key',   'key',       NA,               TRUE,
    'iq',       'limit',     'limit',     '1',              FALSE,
    'iq',       'format',    'format',    'json',           TRUE,
    'geocodio', 'api_key',   'api_key',   NA,               TRUE,
    'geocodio', 'limit',     'limit',     '1',              FALSE,
    'geocodio', 'format',     NA,         NA,               FALSE,
    'census',   'address',   'address',   NA,               TRUE,
    'census',   'format',    'format',    'json',           TRUE,
    'census',   'limit',     NA,          NA,               FALSE,
  )


# Functions for constructing the HTTR::GET queries

# NOTE - does not include api key or address
# Geocodio returns json by default
# Census geocoder is only api that does not offer the limit parameter to limit number of results returned

### Since Code is set up for json we might want to just hardcode the format='json' parameter

query_defaults <- list(
  osm = list(format = 'json', limit = 1),
  census = list(format = 'json', benchmark = 4),
  geocodio = list(limit = 1),
  iq = list(format = 'json', limit = 1)
)

# Default API to use for each service
api_url_defaults <- list(
  geocodio = "https://api.geocod.io/v1.5/geocode",
  osm = "http://nominatim.openstreetmap.org/search",
  iq = "https://us1.locationiq.com/v1/search.php",
  census = "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
)

# Name of the address parameter
address_param_name <- list(
  geocodio = "q",
  osm = "q",
  iq = "q",
  census = "address"  
)

## Address Parameter Name
# OSM: q
# Census: address
# Geocodio: q
# IQ: q 

## API KEY Parameter Name
# Geocodio: api_key
# IQ: key

# query_defaults['osm']


# Construct an an address api query
# api_key only needed for IQ and Geocodio services
get_address_query <- function(method, address, api_key=NULL) {
  
  ## Set Address Parameter ----------------------------
  address_param <- list()
  address_param[[address_param_name[[method]]]] <- address
  
  ## Set Address Parameter with Rules
  # if (method == 'census') address_param <- list(address = address)
  # else address_param <- list(q = address)
  
  ## Set API KEY Parameter ----------------------------
  if (method == 'iq') api_key_param <- list(key = api_key)
  else if (method == 'geocodio')  api_key_param <- list(key = api_key)
  else api_key_param <- list()
  
  # Combine address, api_key, and default parameters for full query
  return( c(address_param, api_key_param, query_defaults[[method]] ) )
}

# Return the API URL for the specified method
get_api_url <- function(method) {
  return( api_url_defaults[[method]] )
}



## -----------------------------------------
  
# Create and Return Completely Custom Query OR build query in steps... (order shouldn't matter)
# 1. Add query defaults
# 2. Add address field
# 3. Add API Key (if necessary)
# 4. Set API URL

