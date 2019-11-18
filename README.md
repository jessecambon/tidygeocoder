# tidygeocoder

[![CRAN](https://www.r-pkg.org/badges/version/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/jessecambon/tidygeocoder/blob/master/LICENSE.md)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/tidygeocoder)](https://CRAN.R-project.org/package=tidygeocoder)

A [tidyverse](https://www.tidyverse.org/)-style geocoder interface for R. Utilizes [US Census](https://geocoding.geo.census.gov/) and [Nominatim](https://nominatim.org) (OSM) geocoder services. Returns latitude and longitude in [tibble format](https://tibble.tidyverse.org/) from addresses. You can find a demo I wrote up on R-Bloggers [here](https://www.r-bloggers.com/geocoding-with-tidygeocoder/).

## Install

To install the stable version from CRAN (the official R package servers):

```
install.packages('tidygeocoder')
```

To install the development version from GitHub:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("jessecambon/tidygeocoder",build_vignettes=TRUE)
```

## Usage
In this brief example, we will use the US Census API to geocode some addresses in the `sample_addresses` dataset.

``` r
library(dplyr)
library(tidygeocoder)

lat_longs <- sample_addresses %>% 
  geocode(addr,lat=latitude,long=longitude)
```

Latitude and longitude columns are attached to our input
dataset. Since we are using the US Census geocoder service, international locations and addresses which are not at the street level (such as cities) are not found.


| name                 | addr                                       | latitude |   longitude |
| :------------------- | :----------------------------------------- | -------: | ----------: |
| White House          | 1600 Pennsylvania Ave Washington, DC       | 38.89875 |  \-77.03535 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | \-122.40314 |
| NA                   | Fake Address                               |       NA |          NA |
| NA                   | NA                                         |       NA |          NA |
|                      |                                            |       NA |          NA |
| US City              | Nashville,TN                               |       NA |          NA |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  \-87.63666 |
| International City   | Nairobi, Kenya                             |       NA |          NA |

Plot our geolocated points:

```r
library(ggplot2)
library(maps)
library(ggrepel)
ggplot(lat_longs %>% filter(!is.na(longitude)),aes(longitude, latitude),color="grey98") +
  borders("state") + theme_classic() + geom_point() +
  theme(line = element_blank(),text = element_blank(),title = element_blank()) +
  geom_label_repel(aes(label =name),show.legend=F) +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)
```
![](us_map.png)

To find international and non-street addresses, we must use the OSM
service. We can use the ‘cascade’ method to attempt to use the US Census
method for each address and only use the OSM service if the Census
method fails (since OSM has a usage limit).

``` r
cascade_points <- sample_addresses %>% 
  geocode(addr,method='cascade')
```

| name                 | addr                                       |        lat |         long | geo\_method |
| :------------------- | :----------------------------------------- | ---------: | ----------: | :---------- |
| White House          | 1600 Pennsylvania Ave Washington, DC       |  38.898754 |  \-77.03535 | census      |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 |  37.794700 | \-122.40314 | census      |
| NA                   | Fake Address                               |         NA |          NA | NA          |
| NA                   | NA                                         |         NA |          NA | NA          |
|                      |                                            |         NA |          NA | NA          |
| US City              | Nashville,TN                               |  36.162230 |  \-86.77435 | osm         |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         |  41.878513 |  \-87.63666 | census      |
| International City   | Nairobi, Kenya                             | \-1.283253 |    36.81724 | osm         |

## References
* [US Census Geocoder](https://geocoding.geo.census.gov/)
* [Nominatim Geocoder](https://nominatim.org)
* [Nominatim Address Check](https://nominatim.openstreetmap.org/)
* [tmaptools](https://cran.r-project.org/package=tmaptools) package (used for OSM geocoding)
* [dplyr](https://dplyr.tidyverse.org/)
* [tidyr](https://tidyr.tidyverse.org)
