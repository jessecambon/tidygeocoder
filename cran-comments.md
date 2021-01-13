## Test environments
* local Ubuntu install, R 4.0.2
* winbuilder r-old devel: `devtools::check_win_oldrelease()`
* winbuilder r-devel : `devtools::check_win_devel()`
* Other environments checked via `rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes 

* Added support for the Google geocoder service ([#34](https://github.com/jessecambon/tidygeocoder/issues/34)) (thanks @chris31415926535).
* An error is now thrown if invalid parameters are passed to geocoder services (the parameters checked are limit, address, street, city, county, state, postalcode, and country) ([#53](https://github.com/jessecambon/tidygeocoder/issues/53)). This behavior can be toggled with the new `param_error` parameter in `geo()` (or `geocode()`).
* Leading zeros on Census FIPs geography columns are now preserved ([#47](https://github.com/jessecambon/tidygeocoder/issues/47)).
* Bug fix for `custom_query` argument with Geocodio batch geocoding ([#48](https://github.com/jessecambon/tidygeocoder/issues/48)).
* Bug fix for vctrs datatype error with cascade method ([#49](https://github.com/jessecambon/tidygeocoder/issues/49)).
* Added more comprehensive testing for internal package functions such as package_addresses, unpackage_addresses, and get_api_query ([#58](https://github.com/jessecambon/tidygeocoder/issues/58)).
* Per CRAN request, `order()` is no longer called on data frames ([#57](https://github.com/jessecambon/tidygeocoder/issues/57)).