### Global package variables

pkg.globals <- new.env()

# These are the input field names for forward geocoding
pkg.globals$address_arg_names <- c('address', 'street', 'city', 'county', 'state', 'postalcode', 'country')

# These are methods that should use single geocoding by default (ie. not batch).
# The reason in this case is that these batch methods are slower.
pkg.globals$single_first_methods <- c("here", "bing")

# These methods do not have limit API arguments, but use the limit parameter in 
# geo() or reverse_geo() to limit the number of results (ie. a passthrough)
pkg.globals$limit_passthru_methods <- c("census", "google")