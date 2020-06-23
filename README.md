
<!-- README.md is generated from README.Rmd. Please edit that file directly and reknit -->

# tidygeocoder <a href='https://jessecambon.github.io/tidygeocoder/'><img src='man/figures/tidygeocoder_hex.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/jessecambon/tidygeocoder/blob/master/LICENSE.md)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/tidygeocoder)](https://CRAN.R-project.org/package=tidygeocoder)
<!-- badges: end -->

## Introduction

Tidygeocoder makes getting data from geocoding services easy. Currently
supported services include the [US
Census](https://geocoding.geo.census.gov/), [Nominatim
(OSM)](https://nominatim.org), [Geocodio](https://www.geocod.io/), and
[Location IQ](https://locationiq.com/). You can find a demo I wrote up
on using this package to plot landmarks in Washington, DC
[here](https://jessecambon.github.io/2019/11/11/tidygeocoder-demo.html).

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

In this brief example, we will use the US Census API to geocode a few
select addresses in the `sample_addresses` dataset.

``` r
library(dplyr)
library(tidygeocoder)

lat_longs <- sample_addresses %>% 
  filter(name %in% c('White House','Transamerica Pyramid','Willis Tower')) %>%
  geocode(addr,lat=latitude,long=longitude)
```

Latitude and longitude columns are attached to our input dataset.

| name                 | addr                                       | latitude |   longitude |
| :------------------- | :----------------------------------------- | -------: | ----------: |
| White House          | 1600 Pennsylvania Ave Washington, DC       | 38.89875 |  \-77.03535 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | \-122.40314 |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  \-87.63666 |

Now that we have the latitude and longitude, we can plot our addresses
with ggplot:

``` r
library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs %>% filter(!is.na(longitude)), aes(longitude, latitude), color="grey99") +
  borders("state") + geom_point() + geom_label_repel(aes(label = name), show.legend=F) + 
  theme_void()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

For further documentation, refer to the [Getting Started
page](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
and the [function
references](https://jessecambon.github.io/tidygeocoder/reference/index.html).

## References

  - [US Census Geocoder](https://geocoding.geo.census.gov/)
  - [Nominatim Geocoder](https://nominatim.org)
  - [Nominatim Address Check](https://nominatim.openstreetmap.org/)
