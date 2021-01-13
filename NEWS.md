# tidygeocoder 1.0.2
* Added support for the Google geocoder service ([#34](https://github.com/jessecambon/tidygeocoder/issues/34)) (thanks @chris31415926535).
* An error is now thrown if invalid parameters are passed to geocoder services (the parameters checked are limit, address, street, city, county, state, postalcode, and country) ([#53](https://github.com/jessecambon/tidygeocoder/issues/53)). This behavior can be toggled with the new `param_error` parameter in `geo()` (or `geocode()`).
* Leading zeros on Census FIPs geography columns are now preserved ([#47](https://github.com/jessecambon/tidygeocoder/issues/47)).
* Bug fix for `custom_query` argument with Geocodio batch geocoding ([#48](https://github.com/jessecambon/tidygeocoder/issues/48)).
* Bug fix for vctrs datatype error with cascade method ([#49](https://github.com/jessecambon/tidygeocoder/issues/49)).
* Added more comprehensive testing for internal package functions such as package_addresses, unpackage_addresses, and get_api_query ([#58](https://github.com/jessecambon/tidygeocoder/issues/58)).
* Per CRAN request, `order()` is no longer called on data frames ([#57](https://github.com/jessecambon/tidygeocoder/issues/57)).

# tidygeocoder 1.0.1
* Fixed an issue that prevented installation on R < 4.0. ([#35](https://github.com/jessecambon/tidygeocoder/issues/35)).
* Updated package documentation. Added examples to utility functions `query_api()` and `get_api_query()`.

# tidygeocoder 1.0.0

### New Functionality
* **New geocoder services**: Support for the Geocodio and Location IQ services has been added. 
* **Batch geocoding** (geocoding multiple addresses per query) is now available for both the Census and Geocodio services.
* **Full results** from the geocoder services can now be returned by using `full_results = TRUE`. This will return all data provided by the geocoder service instead of just latitude and longitude coordinates. Additionally, the `return_type = 'geographies'` argument for the Census geocoder will return geography columns.
* **Address component arguments**: As an alternative to specifying a single-line address, address component arguments are now supported (`street`, `city`, `county`, `postalcode`, `country`).
* **Customizable queries**: Geocoding queries can now be customized using the `limit` and `custom_query` arguments (see the `geo()` function for details).
* **Smart address handling**: Only unique addresses are passed to geocoder services, but the rows in the original data are preserved.
* **Usage limits**: The OSM and IQ services by default are now limited to submitting one query per second (per the `min_time` argument in `geo()`) to respect usage limits. This should fix the past issue of users being locked out of the OSM service due to usage limit violations.
* The `cascade` method can now be customized by using the `cascade_order` argument (see `geo()` documentation)
* **Custom API URLs** can now be specified. This will allow users to specify their own local Nominatim server, for instance.
* The parameters passed to the geocoder service are now displayed to the console when `verbose = TRUE` 

### Under the Hood Improvements
* **Reduced dependencies**: The package has been overhauled so that the only remaining dependencies are `tibble`, `dplyr`, `jsonlite`, and `httr`. The package no longer has direct dependencies on `tmaptools`, `stringr`, `purrr`, `tidyr`, and `rlang`.
* All geocoding queries are now directly executed by `httr`. The inbuilt `api_parameter_reference` dataset is used to map standard "generic" parameter names to the parameter names used by each specific geocoder service. 
* All geocoding functionality has been centralized in the `geo()` function. Users can still use `geocode()`, `geo_osm()`, and `geo_census()` as before. However, `geo_osm()` and `geo_census()` are now just convenience functions that call `geo()` and `geocode()` passes all addresses to `geo()` for geocoding. 

# tidygeocoder 0.2.5

Per CRAN request, fixed an issue where the example for R/geocode.R failed. Only change required was to add a library(dplyr) statement.

# tidygeocoder 0.2.4

Initial CRAN release. Per CRAN request:
* Replaced `print()` with `warning()` to make suppressing console output possible.
* Replaced `\dontrun` with `\donttest` in .R files
