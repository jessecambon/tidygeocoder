# Global package variables


pkg.globals <- new.env()

# These are the input field names for forward geocoding
pkg.globals$address_arg_names <- c("address", "street", "city", "county", "state", "postalcode", "country")

# These are methods that will default to single geocoding by default (ie. not batch).
# because their batch methods are slower.
pkg.globals$single_first_methods <- c("here", "bing")

# These methods do not have limit API arguments, but we still use the limit parameter in
# geo() or reverse_geo() to limit the number of results (ie. a passthrough)
pkg.globals$limit_passthru_methods <- c("census", "google")

# For services that don't offer global coverage, specify geographic limitations
pkg.globals$geographic_limitations <- list(
  "census" = "United States",
  "geocodio" = "United States and Canada"
)

# these methods do not support reverse geocoding
pkg.globals$no_reverse_methods <- c("census")

# these methods do not support flatten=FALSE for batch geocoding
pkg.globals$batch_flatten_required_methods <- c("geocodio", "mapquest")

# these are special api_options parameters that do not correspond to a method
#   init: indicates if this is the first pass through the geo or reverse_geo function
pkg.globals$special_api_options <- c("init")

# default settings for the `api_options` argument in geo() and reverse_geo()
pkg.globals$default_api_options <- list(
  census_return_type = "locations",
  iq_region = "us",
  geocodio_v = 1.7,
  geocodio_hipaa = FALSE,
  mapbox_permanent = FALSE,
  mapquest_open = FALSE
)
