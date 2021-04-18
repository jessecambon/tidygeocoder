# tidygeocoder 1.0.3

### New Features

- Added support for reverse geocoding with the new `reverse_geo()` and `reverse_geocode()` functions. 
- Added support for the [OpenCage](https://opencagedata.com/) geocoder service ([#67](https://github.com/jessecambon/tidygeocoder/issues/67)) (thanks [@dpprdan](https://github.com/dpprdan)).
- Added support for the [HERE](https://developer.here.com/products/geocoding-and-search) ([#74](https://github.com/jessecambon/tidygeocoder/issues/74)), [Mapbox](https://docs.mapbox.com/api/search/) ([#71](https://github.com/jessecambon/tidygeocoder/issues/71)), [MapQuest](https://developer.mapquest.com/documentation/geocoding-api/) ([#85](https://github.com/jessecambon/tidygeocoder/issues/85)),  [TomTom](https://developer.tomtom.com/search-api/search-api-documentation/geocoding) ([#76](https://github.com/jessecambon/tidygeocoder/issues/76)), [Bing](https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/) ([#92](https://github.com/jessecambon/tidygeocoder/issues/92)), and [ArcGIS](https://developers.arcgis.com/rest/geocode/api-reference/overview-world-geocoding-service.htm) ([#98](https://github.com/jessecambon/tidygeocoder/issues/98)) geocoder services (thanks [@dieghernan](https://github.com/dieghernan)). Note that currently the batch geocoding capabilities for the Mapbox and ArcGIS services are not currently implemented (see [#73](https://github.com/jessecambon/tidygeocoder/issues/73) and [#102](https://github.com/jessecambon/tidygeocoder/issues/102)).
    - Added the ` mapbox_permanent`, `here_request_id`, and `mapquest_open` parameters to the `geo()` and `reverse_geo()` functions.
- The `limit` argument can now be used with the "google" and "census" methods to control the number of results returned. These two services do not have limit arguments in their APIs so the limit is applied after the results are returned.
- `batch_limit` is now automatically set according to the specified geocoder service unless otherwise specified.

### Other Changes

- Changed default `method` to `"osm"` (Nominatim) for the `geo()` function (it was previously `"census"`).
- The `geo_<method>` functions are now deprecated.
- Added the `return_input` argument to `geocode()` and `reverse_geocode()` to provide more flexibility when using dataframes as inputs in geocoder queries.
- `limit = NULL` can now be passed to use the default `limit` value for the geocoder service.
- Added the `min_time_reference`, `batch_limit_reference`, `api_key_reference`, and `api_info_reference` datasets to more accessibly store values for `min_time`, `batch_limit`, the names of environmental variables for API keys, and information for documentation (such as API documentation links).
- `geocode()` and `reverse_geocode()` now require `limit = 1` (default) unless `return_input = FALSE`. This fixes a bug where geocoding results could be misaligned with the input dataset when `limit > 1`.  ([#88](https://github.com/jessecambon/tidygeocoder/issues/88)).
- If the number of unique addresses or coordinates exceeds the batch query limit then an error is thrown by default. For forward geocoding, this behavior can be toggled with the new `batch_limit_query` argument in the `geo()` function and `batch_limit_query` is set to FALSE if using the "cascade" method. When `batch_limit_query` is FALSE then the batch query is limited to the batch limit and executed. In the past, all batch queries that exceeded the batch query limit would be limited to the batch limit and no error was thrown.
- The `address_list` argument of `query_api()` has been renamed to `input_list` to reflect that it is used for both forward and reverse queries when using the Geocodio service for batch geocoding.
- The `query_api()` function now returns a named list which contains the response content (`content`) and the HTTP status code (`status`). The `geo()` and `reverse_geo()` functions now use the HTTP status code directly to determine if a response is valid.
- Added [external tests](https://github.com/jessecambon/tidygeocoder/blob/main/external/online_tests.R) to more thoroughly test the package with live queries (internal package tests don't run queries).
- Added functions to generate package documentation from built-in datasets (ex. the methods documentation in `geo()` and `reverse_geo()`).
- Converted package documentation from standard roxygen syntax to Markdown.

# tidygeocoder 1.0.2

- Added support for the [Google](https://developers.google.com/maps/documentation/geocoding/overview) geocoder service ([#34](https://github.com/jessecambon/tidygeocoder/issues/34)) (thanks [@chris31415926535](https://github.com/chris31415926535)).
- An error is now thrown if invalid parameters are passed to geocoder services (the parameters checked are limit, address, street, city, county, state, postalcode, and country) ([#53](https://github.com/jessecambon/tidygeocoder/issues/53)). This behavior can be toggled with the new `param_error` parameter in `geo()` (or `geocode()`).
- Leading zeros on Census FIPs geography columns are now preserved ([\#47](https://github.com/jessecambon/tidygeocoder/issues/47)).
- Bug fix for `custom_query` argument with Geocodio batch geocoding ([\#48](https://github.com/jessecambon/tidygeocoder/issues/48)).
- Bug fix for vctrs datatype error with cascade method ([\#49](https://github.com/jessecambon/tidygeocoder/issues/49)).
- Added more comprehensive testing for internal package functions such as package_addresses, unpackage_addresses, and get_api_query ([#58](https://github.com/jessecambon/tidygeocoder/issues/58)).
- Per CRAN request, `order()` is no longer called on data frames ([\#57](https://github.com/jessecambon/tidygeocoder/issues/57)).

# tidygeocoder 1.0.1

-   Fixed an issue that prevented installation on R \< 4.0. ([\#35](https://github.com/jessecambon/tidygeocoder/issues/35)).
-   Updated package documentation. Added examples to utility functions `query_api()` and `get_api_query()`.

# tidygeocoder 1.0.0

### New Functionality

- **New geocoder services**: Support for the [Geocodio](https://www.geocod.io/) and [Location IQ](https://locationiq.com/) services has been added.
- **Batch geocoding** (geocoding multiple addresses per query) is now available for both the Census and Geocodio services.
- **Full results** from the geocoder services can now be returned by using `full_results = TRUE`. This will return all data provided by the geocoder service instead of just latitude and longitude coordinates. Additionally, the `return_type = 'geographies'` argument for the Census geocoder will return geography columns.
- **Address component arguments**: As an alternative to specifying a single-line address, address component arguments are now supported (`street`, `city`, `county`, `postalcode`, `country`).
- **Customizable queries**: Geocoding queries can now be customized using the `limit` and `custom_query` arguments (see the `geo()` function for details).
- **Smart address handling**: Only unique addresses are passed to geocoder services, but the rows in the original data are preserved.
- **Usage limits**: The OSM and IQ services by default are now limited to submitting one query per second (per the `min_time` argument in `geo()`) to respect usage limits. This should fix the past issue of users being locked out of the OSM service due to usage limit violations.
- The `cascade` method can now be customized by using the `cascade_order` argument (see `geo()` documentation)
- **Custom API URLs** can now be specified. This will allow users to specify their own local Nominatim server, for instance.
- The parameters passed to the geocoder service are now displayed to the console when `verbose = TRUE`

### Under the Hood Improvements

- **Reduced dependencies**: The package has been overhauled so that the only remaining dependencies are `tibble`, `dplyr`, `jsonlite`, and `httr`. The package no longer has direct dependencies on `tmaptools`, `stringr`, `purrr`, `tidyr`, and `rlang`.
- All geocoding queries are now directly executed by `httr`. The inbuilt `api_parameter_reference` dataset is used to map standard "generic" parameter names to the parameter names used by each specific geocoder service.
- All geocoding functionality has been centralized in the `geo()` function. Users can still use `geocode()`, `geo_osm()`, and `geo_census()` as before. However, `geo_osm()` and `geo_census()` are now just convenience functions that call `geo()` and `geocode()` passes all addresses to `geo()` for geocoding.

# tidygeocoder 0.2.5

Per CRAN request, fixed an issue where the example for R/geocode.R failed. Only change required was to add a library(dplyr) statement.

# tidygeocoder 0.2.4

Initial CRAN release. Per CRAN request: \* Replaced `print()` with `warning()` to make suppressing console output possible. \* Replaced `\dontrun` with `\donttest` in .R files
