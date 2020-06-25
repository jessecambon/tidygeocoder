
<!-- README.md is generated from README.Rmd. Please edit that file directly and reknit -->

# tidygeocoder <a href='https://jessecambon.github.io/tidygeocoder/'><img src='man/figures/tidygeocoder_hex.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/jessecambon/tidygeocoder/blob/master/LICENSE.md)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/tidygeocoder)](https://CRAN.R-project.org/package=tidygeocoder)
<!-- badges: end -->

## Introduction

Tidygeocoder makes getting data from geocoding services easy. The
currently supported services are the [US
Census](https://geocoding.geo.census.gov/), [Nominatim
(OSM)](https://nominatim.org), [Geocodio](https://www.geocod.io/), and
[Location IQ](https://locationiq.com/). You can find an example of
geocoding landmarks in Washington, DC
[here](https://jessecambon.github.io/2019/11/11/tidygeocoder-demo.html).

Batch geocoding (passing multiple addresses per query) is supported for
the US Census and Geocodio services. Only unique addresses are passed to
geocoder services to reduce query sizes.

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

In this example we will geocode a few addresses in the
`sample_addresses` dataset using the `geocode()` function and plot them
on a map with ggplot.

``` r
library(dplyr)
library(tidygeocoder)

lat_longs <- sample_addresses %>% 
  filter(name %in% c('White House', 'Transamerica Pyramid', 'Willis Tower')) %>%
  geocode(addr, lat = latitude , long = longitude)
```

The `geocode()` function attaches latitude and longitude columns to our
input dataset of addresses. Note that this code uses the US Census
geocoder since the `method` argument is not specified. To use other
geocoder services, you can specify them with the `method` argument. See
the `geo()` function documentation for details.

| name                 | addr                                       | latitude |   longitude |
| :------------------- | :----------------------------------------- | -------: | ----------: |
| White House          | 1600 Pennsylvania Ave Washington, DC       | 38.89875 |  \-77.03535 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | \-122.40314 |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  \-87.63666 |

We can then plot our addresses on a map using the longitude and latitude
coordinates.

``` r
library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs, aes(longitude, latitude), color="grey99") +
  borders("state") + geom_point() + 
  geom_label_repel(aes(label = name)) + 
  theme_void()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

For further details, refer to the [Getting Started
Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
and the [function
documentation](https://jessecambon.github.io/tidygeocoder/reference/index.html).
