# API URL Functions
# reverse = TRUE for reverse geocoding

## wrapper function for functions below
### IMPORTANT: if arguments are changed in this definition then make sure to 
### update reverse_geo.R and geo.R where this function is called.
get_api_url <- function(method, reverse = FALSE, return_type = 'locations',
                        search = 'onelineaddress', geocodio_v = 1.6, iq_region = 'us', 
                        mapbox_permanent = FALSE, mapquest_open = FALSE, geocodio_hipaa = FALSE) {
  
  api_url <- switch(method,
                    "osm" = get_osm_url(reverse = reverse),
                    "census" = get_census_url(return_type, search),
                    "geocodio" = get_geocodio_url(geocodio_v, reverse = reverse, geocodio_hipaa = geocodio_hipaa),
                    "iq" = get_iq_url(iq_region, reverse = reverse),
                    "opencage" = get_opencage_url(), # same url as forward geocoding
                    "google" = get_google_url(), # same url as forward geocoding
                    "mapbox" = get_mapbox_url(mapbox_permanent), # same url as forward geocoding
                    "here" = get_here_url(reverse = reverse),
                    "tomtom" = get_tomtom_url(reverse = reverse),
                    "mapquest" = get_mapquest_url(mapquest_open, reverse = reverse),
                    "bing" = get_bing_url(),
                    "arcgis" = get_arcgis_url(reverse = reverse),
                    "geoapify" = get_geoapify_url(reverse = reverse)
  )
  
  if (length(api_url) == 0) stop('API URL not found', call. = FALSE)
  return(api_url)
}

##############################################################################################


# return : returntype => 'locations' or 'geographies'
# search:  searchtype => 'onelineaddress', 'addressbatch', 'address', or 'coordinates'
get_census_url <- function(return_type, search) {
  return(paste0("https://geocoding.geo.census.gov/geocoder/", return_type, "/", search))
}

get_geocodio_url <- function(api_v, reverse = FALSE, geocodio_hipaa = FALSE) {
  # return API URL based on api version (ex. 1.6)
  url_base <- if (geocodio_hipaa == TRUE) "https://api-hipaa.geocod.io/v" else "https://api.geocod.io/v"
  url_keyword <- if (reverse == TRUE) 'reverse' else 'geocode'
  return(paste0(url_base, as.character(api_v), "/", url_keyword))
}

get_osm_url <- function(reverse = FALSE) {
  url_keyword <- if (reverse == TRUE) 'reverse' else 'search'
  return(paste0('https://nominatim.openstreetmap.org/', url_keyword))
}

get_iq_url <- function(region = 'us', reverse = FALSE) {
  # region can be 'us' or 'eu'
  url_keyword <- if (reverse == TRUE) 'reverse' else 'search'
  return(paste0("https://", region, "1.locationiq.com/v1/", url_keyword,  ".php"))
}

get_google_url <- function() return("https://maps.googleapis.com/maps/api/geocode/json")

get_opencage_url <- function() return("https://api.opencagedata.com/geocode/v1/json")

get_mapbox_url <- function(mapbox_permanent = FALSE) {
  endpoint <- if (mapbox_permanent == TRUE) "mapbox.places-permanent" else "mapbox.places"
  return(paste0("https://api.mapbox.com/geocoding/v5/", endpoint, "/"))
}

get_here_url <- function(reverse = FALSE) {
  if (reverse == TRUE) return("https://revgeocode.search.hereapi.com/v1/revgeocode")
  return("https://geocode.search.hereapi.com/v1/geocode")
}

get_tomtom_url <- function(reverse = FALSE) {
  url_keyword <- if (reverse == TRUE) 'reverseGeocode/' else 'geocode/'
  return(paste0('https://api.tomtom.com/search/2/', url_keyword))
}

get_mapquest_url <- function(mapquest_open = FALSE, reverse = FALSE) {
  endpoint <- if (mapquest_open == TRUE) 'http://open.' else 'http://www.'
  url_keyword <- if (reverse == TRUE) 'reverse' else 'address'
  return(paste0(endpoint,'mapquestapi.com/geocoding/v1/', url_keyword))
}

get_bing_url <- function() {
  return('http://dev.virtualearth.net/REST/v1/Locations')
}

get_arcgis_url <- function(reverse = FALSE) {
  if (reverse == TRUE) return('https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode')
  else return('https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates')
}

get_geoapify_url <- function(reverse = FALSE) {
  endpoint <- if (reverse) 'reverse' else 'search'
  return(paste0('https://api.geoapify.com/v1/geocode/', endpoint))
}
