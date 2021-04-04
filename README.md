
<!-- README.md is generated from README.Rmd. Please edit that file directly and reknit -->

# tidygeocoder <a href='https://jessecambon.github.io/tidygeocoder/'><img src="man/figures/tidygeocoder_hex.png" align="right" height="139"/></a>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/jessecambon/tidygeocoder/blob/master/LICENSE.md)
[![CRAN](https://www.r-pkg.org/badges/version/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![CRAN Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tidygeocoder)](https://CRAN.R-project.org/package=tidygeocoder)
[![CRAN Downloads Per
Month](http://cranlogs.r-pkg.org/badges/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4448251.svg)](https://doi.org/10.5281/zenodo.4448251)
<!--[![Github Stars](https://img.shields.io/github/stars/jessecambon/tidygeocoder?style=social&label=Github)](https://github.com/jessecambon/tidygeocoder) -->

<!-- badges: end -->

## Introduction

Tidygeocoder makes getting data from geocoder services easy. A unified
interface is provided for the geocoder services listed below and all
results are returned in [tibble](https://tibble.tidyverse.org/)
(dataframe) format.

**Features:**

-   Both **forward geocoding** (addresses ⮕ coordinates) and **reverse
    geocoding** (coordinates ⮕ addresses) are supported.
-   **Batch geocoding** (geocoding multiple addresses or coordinates in
    a single query) is used by default if supported by the geocoder
    service when multiple inputs (addresses or coordinates) are provided
    (with some noted exceptions for services with slower batch
    geocoding).
-   Duplicate, missing/NA, and blank input data is handled elegantly -
    only unique inputs are submitted in queries to geocoder services,
    but the rows in the original data are preserved by default.

**References:**

-   [Blog posts on
    tidygeocoder](https://jessecambon.github.io/tag/tidygeocoder) for
    updates on releases and more usage examples. In particular [this
    post](https://jessecambon.github.io/2020/07/15/tidygeocoder-1-0-0.html)
    and [this
    post](https://jessecambon.github.io/2019/11/11/tidygeocoder-demo.html)
    demonstrate some relevant mapping workflows.
-   [Getting Started
    Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
    for more detailed and comprehensive usage examples.
-   [API Reference
    Page](https://jessecambon.github.io/tidygeocoder/articles/api_reference.html)
    to view the built-in datasets that are used by tidygeocoder to set
    key parameters and settings for geocoder queries.

## Installation

To install the stable version from CRAN (the official R package
servers):

``` r
install.packages('tidygeocoder')
```

Alternatively, you can install the latest development version from
GitHub:

``` r
devtools::install_github("jessecambon/tidygeocoder")
```

## Geocoder Services

The supported geocoder services are shown in the table below. The
“method” is used to specify the geocoder service in tidygeocoder
functions such as `geo()` and `reverse_geo()`. Refer to the [API
Reference
page](https://jessecambon.github.io/tidygeocoder/articles/api_reference.html)
and to each service’s API documentation for more details.

| Service                                                                                                 | Method     | API Key Required | Batch Geocoding | Usage Limitations                   | API Documentation                                                                                                                       |
|:--------------------------------------------------------------------------------------------------------|:-----------|:-----------------|:----------------|:------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------|
| [US Census](https://geocoding.geo.census.gov/)                                                          | `census`   |                  | ✅               |                                     | [docs](https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html) |
| [Nominatim](https://nominatim.org)                                                                      | `osm`      |                  |                 | 1 query per second                  | [docs](https://nominatim.org/release-docs/develop/api/Search/)                                                                          |
| [ArcGIS](https://developers.arcgis.com/rest/geocode/api-reference/overview-world-geocoding-service.htm) | `arcgis`   |                  |                 |                                     | [docs](https://developers.arcgis.com/rest/geocode/api-reference/overview-world-geocoding-service.htm)                                   |
| [Geocodio](https://www.geocod.io/)                                                                      | `geocodio` | ✅                | ✅               | 1000 queries per minute (free tier) | [docs](https://www.geocod.io/docs/)                                                                                                     |
| [Location IQ](https://locationiq.com/)                                                                  | `iq`       | ✅                |                 | 2 queries per second (free tier)    | [docs](https://locationiq.com/docs)                                                                                                     |
| [Google](https://developers.google.com/maps/documentation/geocoding/overview)                           | `google`   | ✅                |                 | 50 queries per second               | [docs](https://developers.google.com/maps/documentation/geocoding/overview)                                                             |
| [OpenCage](https://opencagedata.com)                                                                    | `opencage` | ✅                |                 | 1 query/second                      | [docs](https://opencagedata.com/api)                                                                                                    |
| [Mapbox](https://docs.mapbox.com/api/search/)                                                           | `mapbox`   | ✅                |                 | 600 queries per minute (free tier)  | [docs](https://docs.mapbox.com/api/search/geocoding/)                                                                                   |
| [HERE](https://developer.here.com/products/geocoding-and-search)                                        | `here`     | ✅                | ✅               | 5 queries per second (free tier)    | [docs](https://developer.here.com/documentation/geocoding-search-api/dev_guide/index.html)                                              |
| [TomTom](https://developer.tomtom.com/search-api/search-api-documentation/geocoding)                    | `tomtom`   | ✅                | ✅               | 5 queries per second (free tier)    | [docs](https://developer.tomtom.com/search-api/search-api-documentation-geocoding/geocode)                                              |
| [MapQuest](https://developer.mapquest.com/documentation/geocoding-api/)                                 | `mapquest` | ✅                | ✅               |                                     | [docs](https://developer.mapquest.com/documentation/geocoding-api/)                                                                     |
| [Bing](https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/)                              | `bing`     | ✅                | ✅               |                                     | [docs](https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/)                                                              |

**Highlights:**

-   The US Census service does not support reverse geocoding and is
    limited to the United States.
-   Geocodio is limited to the United States and Canada.
-   The Census and Nominatim (“osm”) services are free and do not
    require an API key.
-   ArcGIS can be used for free without an API key, but some features
    require a paid subscription and API key.
-   The Geocodio, Location IQ, OpenCage, Mapbox, HERE, TomTom, MapQuest,
    and Bing are commercial services that offer free tiers.
-   The Google service has no free tier and [bills per
    query](https://developers.google.com/maps/documentation/geocoding/usage-and-billing).

**Notes:**

-   The US Census service supports street-level addresses only (ie. “11
    Wall St New York, NY” is OK but “New York, NY” is not).
-   The Mapbox service is capable of performing batch geocoding when
    using the [permanent
    endpoint](https://docs.mapbox.com/api/search/geocoding/#batch-geocoding),
    but this capability is not currently implemented in tidygeocoder. If
    you’d like to add this capability to the package see [issue
    \#73](https://github.com/jessecambon/tidygeocoder/issues/73).
-   The ArcGIS service is capable of performing [batch
    geocoding](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm)
    but this capability is not currently implemented in tidygeocoder. If
    you’d like to add this capability see
    [\#102](https://github.com/jessecambon/tidygeocoder/issues/102).
-   For the ArcGIS service, an API Key is not strictly required if the
    service is used for search capabilities only (see [Free vs. paid
    operations](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-free-vs-paid.htm)).
    It is possible to include an API Key on the request via the
    `custom_query` parameter:

``` r
tidygeocoder::geo(address = "New York, USA", method = "arcgis",
  custom_query = list(token = "<API_KEY>"))
```

## Usage

In this example we will geocode a few addresses using the `geocode()`
function and plot them on a map with ggplot.

``` r
library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
~name,                  ~addr,
"White House",          "1600 Pennsylvania Ave NW, Washington, DC",
"Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
"Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)
```

The `geocode()` function attaches latitude and longitude columns to our
input dataset of addresses. The [Nominatim
(“osm”)](https://nominatim.org/) geocoder is used here, but other
services can be specified with the `method` argument. See the `geo()`
function documentation for details.

| name                 | addr                                       | latitude |  longitude |
|:---------------------|:-------------------------------------------|---------:|-----------:|
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89770 |  -77.03655 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79520 | -122.40279 |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87887 |  -87.63591 |

Now that we have the longitude and latitude coordinates, we can use
ggplot to plot our addresses on a map.

``` r
library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() + 
  geom_label_repel(aes(label = name)) + 
  theme_void()
```

<img src="man/figures/README-usamap-1.png" style="display: block; margin: auto;" />

To return the full results from a geocoder service (not just latitude
and longitude) you can use `full_results = TRUE`. Additionally, for the
Census geocoder you can use `return_type = 'geographies'` to return
geography columns (state, county, Census tract, and Census block).

``` r
full <- some_addresses %>%
  geocode(addr, method = 'census', full_results = TRUE, 
          return_type = 'geographies')
```

| name                 | addr                                       |      lat |       long |  id | input\_address                                  | match\_indicator | match\_type | matched\_address                                | tiger\_line\_id | tiger\_side | state\_fips | county\_fips | census\_tract | census\_block |
|:---------------------|:-------------------------------------------|---------:|-----------:|----:|:------------------------------------------------|:-----------------|:------------|:------------------------------------------------|:----------------|:------------|:------------|:-------------|:--------------|:--------------|
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89875 |  -77.03535 |   1 | 1600 Pennsylvania Ave NW, Washington, DC, , ,   | Match            | Exact       | 1600 PENNSYLVANIA AVE NW, WASHINGTON, DC, 20500 | 76225813        | L           | 11          | 001          | 980000        | 1034          |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | -122.40314 |   2 | 600 Montgomery St, San Francisco, CA 94111, , , | Match            | Exact       | 600 MONTGOMERY ST, SAN FRANCISCO, CA, 94111     | 192281262       | R           | 06          | 075          | 061101        | 2014          |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  -87.63666 |   3 | 233 S Wacker Dr, Chicago, IL 60606, , ,         | Match            | Exact       | 233 S WACKER DR, CHICAGO, IL, 60606             | 112050003       | L           | 17          | 031          | 839100        | 2008          |

To perform **reverse geocoding** (obtaining addresses from latitude and
longitude coordinates), we can use the `reverse_geocode()` function. The
arguments are similar to the `geocode()` function, but we now are
specifying the latitude and longitude columns in our dataset with the
`lat` and `long` arguments. The single line address is returned in a
column named by the `address` argument. See the `reverse_geo()` function
documentation for more details on reverse geocoding.

``` r
rev1 <- lat_longs %>%
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)
```

| name                 | addr                                       | latitude |  longitude | address\_found                                                                                                                                   | place\_id | licence                                                                  | osm\_type |   osm\_id | osm\_lat           | osm\_lon            | historic    | house\_number | road                          | city          | state                | postcode | country       | country\_code | boundingbox                                          | tourism              | neighbourhood      | county                        | building     | suburb |
|:---------------------|:-------------------------------------------|---------:|-----------:|:-------------------------------------------------------------------------------------------------------------------------------------------------|----------:|:-------------------------------------------------------------------------|:----------|----------:|:-------------------|:--------------------|:------------|:--------------|:------------------------------|:--------------|:---------------------|:---------|:--------------|:--------------|:-----------------------------------------------------|:---------------------|:-------------------|:------------------------------|:-------------|:-------|
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89770 |  -77.03655 | White House, 1600, Pennsylvania Avenue Northwest, Washington, District of Columbia, 20500, United States                                         | 147370893 | Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright> | way       | 238241022 | 38.897699700000004 | -77.03655315        | White House | 1600          | Pennsylvania Avenue Northwest | Washington    | District of Columbia | 20500    | United States | us            | 38.8974908 , 38.897911 , -77.0368537, -77.0362519    | NA                   | NA                 | NA                            | NA           | NA     |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79520 | -122.40279 | Transamerica Pyramid, 600, Montgomery Street, Financial District, San Francisco, San Francisco City and County, California, 94111, United States |  95364489 | Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright> | way       |  24222973 | 37.795200550000004 | -122.40279267840137 | NA          | 600           | Montgomery Street             | San Francisco | California           | 94111    | United States | us            | 37.7948854 , 37.7954472 , -122.4031399, -122.4024317 | Transamerica Pyramid | Financial District | San Francisco City and County | NA           | NA     |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87887 |  -87.63591 | Willis Tower, 233, South Wacker Drive, Printer’s Row, Loop, Chicago, Cook County, Illinois, 60606, United States                                 | 103673983 | Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright> | way       |  58528804 | 41.878871700000005 | -87.63590893936448  | NA          | 233           | South Wacker Drive            | Chicago       | Illinois             | 60606    | United States | us            | 41.8785389 , 41.8791932 , -87.6363362, -87.6354746   | NA                   | Printer’s Row      | Cook County                   | Willis Tower | Loop   |

For further documentation, refer to the [Getting Started
Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
and the [function
documentation](https://jessecambon.github.io/tidygeocoder/reference/index.html).

## Contributing

Contributions to the tidygeocoder package are welcome. File [an
issue](https://github.com/jessecambon/tidygeocoder/issues) for bug fixes
or suggested features. If you would like to add support for a new
geocoder service, reference [this
page](https://jessecambon.github.io/tidygeocoder/docs/articles/adding_geocoder_services.html)
for instructions.

## Citing tidygeocoder

``` r
citation('tidygeocoder')
```

</br>

<blockquote>

``` r
citation('tidygeocoder')

To cite tidygeocoder in publications use:

  Jesse Cambon, Christopher Belanger (2021). tidygeocoder: Geocoding
  Made Easy (version 1.0.2). DOI: 10.5281/zenodo.4448251. URL:
  https://CRAN.R-project.org/package=tidygeocoder

A BibTeX entry for LaTeX users is

  @Misc{,
    title = {tidygeocoder: Geocoding Made Easy},
    author = {Jesse Cambon and Christopher Belanger},
    year = {2021},
    publisher = {Zenodo},
    note = {R package version 1.0.2},
    url = {https://CRAN.R-project.org/package=tidygeocoder},
    doi = {10.5281/zenodo.4448251},
  }
```

</blockquote>

Or refer to the [citation
page](https://jessecambon.github.io/tidygeocoder/authors.html).
