
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
<!--[![Github Stars](https://img.shields.io/github/stars/jessecambon/tidygeocoder?style=social&label=Github)](https://github.com/jessecambon/tidygeocoder) -->
<!-- badges: end -->

## Introduction

Tidygeocoder makes getting data from geocoder services easy. In addition
to the usage example below you can find a post on making a map of
European soccer club stadiums
[here](https://jessecambon.github.io/2020/07/15/tidygeocoder-1-0-0.html),
a post on mapping landmarks in Washington, DC
[here](https://jessecambon.github.io/2019/11/11/tidygeocoder-demo.html),
and a vignette with more detailed usage examples
[here](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html).

All results are returned in [tibble
format](https://tibble.tidyverse.org/). Batch geocoding (geocoding
multiple addresses per query) is used by default for the US Census and
Geocodio services when multiple addresses are provided. Duplicate,
missing/NA, and blank address data is handled elegantly - only unique
addresses are passed to geocoder services, but the rows in the original
data are preserved.

## Geocoder Services

The currently supported services are the [US Census
geocoder](https://geocoding.geo.census.gov/), [Nominatim
(OSM)](https://nominatim.org), [Geocodio](https://www.geocod.io/), and
[Location IQ](https://locationiq.com/). The Census geocoder is
restricted to street-level addresses in the United States, Geocodio
covers the U.S. and Canada, while Location IQ and OSM have worldwide
coverage. The Census and OSM services support batch geocoding (Location
IQ and OSM do not).

The Census and OSM services are free; Geocodio and Location IQ are
commercial services that require API keys, but also offer free usage
tiers. OSM and Location IQ both have usage frequency limits. Refer to
the documentation of each service for more details.

## Installation

To install the stable version from CRAN (the official R package
servers):

``` r
install.packages('tidygeocoder')
```

Alternatively you can install the development version from GitHub:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("jessecambon/tidygeocoder")
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
"White House",          "1600 Pennsylvania Ave, Washington, DC",
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
| White House          | 1600 Pennsylvania Ave, Washington, DC      | 38.89875 |  \-77.03535 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | \-122.40314 |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  \-87.63666 |

Now that we have the longitude and latitude coordinates, we can use
ggplot to plot our addresses on a map.

``` r
library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs, aes(longitude, latitude), color="grey99") +
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
#> $ addr            <chr> "1600 Pennsylvania Ave, Washington, DC", "600 Montgom…
#> $ lat             <dbl> 38.89875, 37.79470, 41.87851
#> $ long            <dbl> -77.03535, -122.40314, -87.63666
#> $ id              <int> 1, 2, 3
#> $ input_address   <chr> "1600 Pennsylvania Ave, Washington, DC, , , ", "600 M…
#> $ match_indicator <chr> "Match", "Match", "Match"
#> $ match_type      <chr> "Non_Exact", "Exact", "Exact"
#> $ matched_address <chr> "1600 PENNSYLVANIA AVE NW, WASHINGTON, DC, 20006", "6…
#> $ tiger_line_id   <int> 76225813, 192281262, 112050003
#> $ tiger_side      <chr> "L", "R", "L"
#> $ state_fips      <int> 11, 6, 17
#> $ county_fips     <int> 1, 75, 31
#> $ census_tract    <int> 6202, 61100, 839100
#> $ census_block    <int> 1031, 1013, 2006
```

For further documentation, refer to the [Getting Started
Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
and the [function
documentation](https://jessecambon.github.io/tidygeocoder/reference/index.html).
