## Test environments
* local Ubuntu install, R 4.0.5
* GitHub Actions [R-CMD-check](https://github.com/jessecambon/tidygeocoder/blob/main/.github/workflows/check-full.yaml)
* winbuilder r-old devel: `devtools::check_win_oldrelease()`
* winbuilder r-devel : `devtools::check_win_devel()`
* Other environments checked via `rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

## R CMD check results

0 errors | 0 warnings | 1 notes

## Notes 

- RE the NOTE, the DOI is reserved and will become active once released.
- Per request, fixed improper link in man/louisville.Rd
- Updated DESCRIPTION to attmept to fix WARNING in CRAN RE vignette rebuilding

## New Features

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