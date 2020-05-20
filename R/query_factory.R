# Functions for constructing the HTTR::GET queries

# NOTE - does not include api key or address
# Geocodio returns json by default
# Census geocoder is only api that does not offer the limit parameter to limit number of results returned

### Since Code is set up for json we might want to just hardcode the format='json' parameter

query_defaults <- list(
  osm = list(format = 'json', limit=1),
  census = list(format = 'json', benchmark = 4),
  geocodio = list(limit = 1),
  iq = list(format = 'json', limit = 1)
)

api_url_defaults <- list(
  geocodio = "https://api.geocod.io/v1.5/geocode",
  osm = "http://nominatim.openstreetmap.org/search",
  iq = "https://us1.locationiq.com/v1/search.php",
  census = "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
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

construct_query <- function() {
  
}
  
# Create and Return Completely Custom Query OR build query in steps... (order shouldn't matter)
# 1. Add query defaults
# 2. Add address field
# 3. Add API Key (if necessary)
# 4. Set API URL

