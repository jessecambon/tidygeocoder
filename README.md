# tidygeocoder

A tidyverse-style geocoder interface for R. Utilizes US Census and Nominatim (OSM) geocoder services. Returns latitude and longitude in tibble format from addresses.

## Installation

To download, install, and load package from github:

```
install.packages('devtools')
library(devtools)
install_github("jessecambon/tidygeocoder",build_vignettes=TRUE)
library(tidygeocoder)
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
* US Census Geocoder: https://geocoding.geo.census.gov/
* Nominatim (OSM): https://nominatim.openstreetmap.org/ and https://nominatim.org
* tmaptools package (used for OSM geocoding): https://cran.r-project.org/package=tmaptools
* dplyr: https://dplyr.tidyverse.org/


# Developer Notes
* The man documents are created with roxygen2 based on `R/` directory code files. Run this with CTRL+SHIFT+D (in RStudio) or `royxenize()` before building the package.
* Test package with `devtools::test()` (see `/tests` directory)
* Use `devtools::check()` to check for issues (also runs `devtools::test()`)
* Use `devtools::build()` to build the package with vignettes included (creates .tar.gz file)
* Use `install.packages(path_to_file, repos = NULL)` to load the package. May need to close out and reopen RStudio to see changes in vignettes and for cross-references in documentation to work. It also may be necessary to knit the vignette `.Rmd` file from a fresh RStudio session (close out the project) to update the HTML file.

## Development Resources
* General Instructions: http://r-pkgs.had.co.nz/
* More general instructions: https://rstats-pkgs.readthedocs.io
* roxygen2 : https://cran.r-project.org/web/packages/roxygen2/
* Devtools cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf

## Todo/Backlog
* Make `geocode()` function able to take a vector input (no dataframe)?
* Classify address as street level, zip, city level, US/vs non-US, etc. and hand it to the proper geocoder
* Warning for when user has hit usage limit on OSM
* Progress bar?
