# Functions for constructing the HTTR::GET queries


### Census

census_query <- list(address = address, format = 'json', benchmark = benchmark)


### OSM

# addressdetails = 0/1
# limit = 1 means we only return one query result
osm_query <- list(q = address, format = 'json', limit = 1 )


### Geocodio

# Geocodio returns json by default
list(q = address,api_key = geocodio_api_key,limit = 1)