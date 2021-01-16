
<!-- README.md is generated from README.Rmd. Please edit that file directly and reknit -->

# tidygeocoder <a href='https://jessecambon.github.io/tidygeocoder/'><img src='man/figures/tidygeocoder_hex.png' align="right" height="139" /></a>

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
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3981510.svg)](https://doi.org/10.5281/zenodo.3981510)
<!--[![Github Stars](https://img.shields.io/github/stars/jessecambon/tidygeocoder?style=social&label=Github)](https://github.com/jessecambon/tidygeocoder) -->
<!-- badges: end -->

## Introduction

Tidygeocoder makes getting data from geocoder services easy. A unified
interface is provided for the supported geocoder services listed below.
All results are returned in [tibble
format](https://tibble.tidyverse.org/).

Batch geocoding (geocoding multiple addresses per query) is used by
default if possible when multiple addresses are provided. Duplicate,
missing/NA, and blank address data is handled elegantly - only unique
addresses are passed to geocoder services, but the rows in the original
data are preserved by default.

In addition to the usage example below you can refer to the following
references:

  - [Mapping European soccer club
    stadiums](https://jessecambon.github.io/2020/07/15/tidygeocoder-1-0-0.html)
  - [Mapping Washington, DC
    landmarks](https://jessecambon.github.io/2019/11/11/tidygeocoder-demo.html)
  - [Getting Started
    Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
    for more detailed and comprehensive usage examples. The last section
    of the vignette contains a [helpful
    reference](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html#api-reference-1)
    on geocoder service parameters.

## Installation

To install the stable version from CRAN (the official R package
servers):

``` r
install.packages('tidygeocoder')
```

Alternatively, you can install the latest development version from
GitHub:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("jessecambon/tidygeocoder")
```

## Geocoder Services

The supported geocoder services are shown in the table below with their
geographic limitations, if they support batch geocoding (geocoding
multiple addresses in a single query), if an API key is required, and
the usage rate limitations. Refer to the website for each geocoder
service for the most up-to-date details on costs, capabilities, and
usage limitations.

| Service                                                                       | Geography     | Batch Geocoding | API Key Required | Query Rate Limit        |
| ----------------------------------------------------------------------------- | ------------- | --------------- | ---------------- | ----------------------- |
| [US Census](https://geocoding.geo.census.gov/)                                | US            | Yes             | No               | N/A                     |
| [Nominatim (OSM)](https://nominatim.org)                                      | Worldwide     | No              | No               | 1/second                |
| [Geocodio](https://www.geocod.io/)                                            | US and Canada | Yes             | Yes              | 1000/minute (free tier) |
| [Location IQ](https://locationiq.com/)                                        | Worldwide     | No              | Yes              | 2/second (free tier)    |
| [Google](https://developers.google.com/maps/documentation/geocoding/overview) | Worldwide     | No              | Yes              | 50/second               |

Note that:

  - The US Census service supports street-level addresses only (ie. “11
    Wall St New York, NY” is OK but “New York, NY” is not).
  - Nominatim (OSM) and Geocodio both support a maximum of 10,000
    addresses per batch query.
  - The Census and OSM services are free while Geocodio and Location IQ
    are commercial services that offer both free and paid usage tiers.
    The Google service [bills per
    query](https://developers.google.com/maps/documentation/geocoding/usage-and-billing).

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
  geocode(addr, method = 'census', lat = latitude , long = longitude)
```

The `geocode()` function attaches latitude and longitude columns to our
input dataset of addresses. The US Census geocoder is used here, but
other services can be specified with the `method` argument. See the
`geo()` function documentation for details.

| name                 | addr                                       | latitude |   longitude |
| :------------------- | :----------------------------------------- | -------: | ----------: |
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89875 |  \-77.03535 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | \-122.40314 |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  \-87.63666 |

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
  geocode(addr, method = 'census', full_results = TRUE, return_type = 'geographies')

glimpse(full)
#> Rows: 3
#> Columns: 15
#> $ name            <chr> "White House", "Transamerica Pyramid", "Willis Tower"
#> $ addr            <chr> "1600 Pennsylvania Ave NW, Washington, DC", "600 Mont…
#> $ lat             <dbl> 38.89875, 37.79470, 41.87851
#> $ long            <dbl> -77.03535, -122.40314, -87.63666
#> $ id              <int> 1, 2, 3
#> $ input_address   <chr> "1600 Pennsylvania Ave NW, Washington, DC, , , ", "60…
#> $ match_indicator <chr> "Match", "Match", "Match"
#> $ match_type      <chr> "Exact", "Exact", "Exact"
#> $ matched_address <chr> "1600 PENNSYLVANIA AVE NW, WASHINGTON, DC, 20500", "6…
#> $ tiger_line_id   <chr> "76225813", "192281262", "112050003"
#> $ tiger_side      <chr> "L", "R", "L"
#> $ state_fips      <chr> "11", "06", "17"
#> $ county_fips     <chr> "001", "075", "031"
#> $ census_tract    <chr> "980000", "061101", "839100"
#> $ census_block    <chr> "1034", "2014", "2008"
```

For further documentation, refer to the [Getting Started
Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
and the [function
documentation](https://jessecambon.github.io/tidygeocoder/reference/index.html).

## Contributing

Contributions to the tidygeocoder package are welcome. File [an
issue](https://github.com/jessecambon/tidygeocoder/issues) for bug fixes
or suggested features. If you would like to add support for a new
geocoder service, reference [this
post](https://github.com/jessecambon/tidygeocoder/issues/62#issue-777707424)
for instructions.
